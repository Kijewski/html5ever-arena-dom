// Copyright 2021 René Kijewski and the html5ever Project Developers.
// See the COPYRIGHT file at the top-level directory of this distribution.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

#![forbid(unsafe_code)]
#![deny(missing_docs)]

//! Stream generated HTML data into [html5ever] to tidy it up while generating the data.

use std::borrow::Cow;
use std::cell::{Cell, RefCell};
use std::collections::{HashSet, VecDeque};
use std::io::Write;
use std::{fmt, io, ptr};

use html5ever::driver::{parse_document, ParseOpts};
use html5ever::interface::tree_builder::{ElementFlags, NodeOrText, QuirksMode, TreeSink};
use html5ever::serialize::{HtmlSerializer, Serialize, SerializeOpts, Serializer, TraversalScope};
use html5ever::tendril::{StrTendril, TendrilSink};
use html5ever::tree_builder::TreeBuilderOpts;
use html5ever::{Attribute, ExpandedName, Parser, QualName};

// Copied and adapted from
// https://github.com/servo/html5ever/blob/cfea19f74d922ad049614cdb88a7805bc4ea7e06/html5ever/examples/arena.rs
// {

// Copyright 2014-2017 The html5ever Project Developers. See the
// COPYRIGHT file at the top-level directory of this distribution.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

/// Allocator for nodes in the HTML to render.
pub type Arena<'arena> = &'arena typed_arena::Arena<Node<'arena>>;

/// A (serializable) reference to a node in the HTML to render.
pub type Ref<'arena> = &'arena Node<'arena>;

type Link<'arena> = Cell<Option<Ref<'arena>>>;

/// An HTML document tree that uses a typed arena to store its nodes.
pub struct ArenaSink<'arena> {
    arena: Arena<'arena>,
    document: Ref<'arena>,
}

#[doc(hidden)]
pub struct Node<'arena> {
    parent: Link<'arena>,
    next_sibling: Link<'arena>,
    previous_sibling: Link<'arena>,
    first_child: Link<'arena>,
    last_child: Link<'arena>,
    data: NodeData<'arena>,
}

enum NodeData<'arena> {
    Document,
    Doctype {
        name: StrTendril,
    },
    Text {
        contents: RefCell<StrTendril>,
    },
    Comment {
        contents: StrTendril,
    },
    Element {
        name: QualName,
        attrs: RefCell<Vec<Attribute>>,
        template_contents: Option<Ref<'arena>>,
        mathml_annotation_xml_integration_point: bool,
    },
    ProcessingInstruction {
        target: StrTendril,
        contents: StrTendril,
    },
}

impl<'arena> Node<'arena> {
    fn new(data: NodeData<'arena>) -> Self {
        Node {
            parent: Cell::new(None),
            previous_sibling: Cell::new(None),
            next_sibling: Cell::new(None),
            first_child: Cell::new(None),
            last_child: Cell::new(None),
            data,
        }
    }

    fn detach(&self) {
        let parent = self.parent.take();
        let previous_sibling = self.previous_sibling.take();
        let next_sibling = self.next_sibling.take();

        if let Some(next_sibling) = next_sibling {
            next_sibling.previous_sibling.set(previous_sibling);
        } else if let Some(parent) = parent {
            parent.last_child.set(previous_sibling);
        }

        if let Some(previous_sibling) = previous_sibling {
            previous_sibling.next_sibling.set(next_sibling);
        } else if let Some(parent) = parent {
            parent.first_child.set(next_sibling);
        }
    }

    fn append(&'arena self, new_child: &'arena Self) {
        new_child.detach();
        new_child.parent.set(Some(self));
        if let Some(last_child) = self.last_child.take() {
            new_child.previous_sibling.set(Some(last_child));
            debug_assert!(last_child.next_sibling.get().is_none());
            last_child.next_sibling.set(Some(new_child));
        } else {
            debug_assert!(self.first_child.get().is_none());
            self.first_child.set(Some(new_child));
        }
        self.last_child.set(Some(new_child));
    }

    fn insert_before(&'arena self, new_sibling: &'arena Self) {
        new_sibling.detach();
        new_sibling.parent.set(self.parent.get());
        new_sibling.next_sibling.set(Some(self));
        if let Some(previous_sibling) = self.previous_sibling.take() {
            new_sibling.previous_sibling.set(Some(previous_sibling));
            debug_assert!(ptr::eq::<Node>(
                previous_sibling.next_sibling.get().unwrap(),
                self
            ));
            previous_sibling.next_sibling.set(Some(new_sibling));
        } else if let Some(parent) = self.parent.get() {
            debug_assert!(ptr::eq::<Node>(parent.first_child.get().unwrap(), self));
            parent.first_child.set(Some(new_sibling));
        }
        self.previous_sibling.set(Some(new_sibling));
    }
}

impl<'arena> ArenaSink<'arena> {
    fn new_node(&self, data: NodeData<'arena>) -> Ref<'arena> {
        self.arena.alloc(Node::new(data))
    }

    fn append_common<P, A>(&self, child: NodeOrText<Ref<'arena>>, previous: P, append: A)
    where
        P: FnOnce() -> Option<Ref<'arena>>,
        A: FnOnce(Ref<'arena>),
    {
        let new_node = match child {
            NodeOrText::AppendText(text) => {
                // Append to an existing Text node if we have one.
                if let Some(&Node {
                    data: NodeData::Text { ref contents },
                    ..
                }) = previous()
                {
                    contents.borrow_mut().push_tendril(&text);
                    return;
                }
                self.new_node(NodeData::Text {
                    contents: RefCell::new(text),
                })
            }
            NodeOrText::AppendNode(node) => node,
        };

        append(new_node)
    }

    /// Construct a new [ArenaSink] that uses `arena` as its memory space.
    #[inline]
    pub fn new(arena: Arena<'arena>) -> Self {
        Self {
            arena,
            document: arena.alloc(Node::new(NodeData::Document)),
        }
    }
}

impl<'arena> TreeSink for ArenaSink<'arena> {
    type Handle = Ref<'arena>;
    type Output = Ref<'arena>;

    fn finish(self) -> Ref<'arena> {
        self.document
    }

    fn parse_error(&mut self, _: Cow<'static, str>) {}

    fn get_document(&mut self) -> Ref<'arena> {
        self.document
    }

    fn set_quirks_mode(&mut self, _mode: QuirksMode) {}

    fn same_node(&self, x: &Ref<'arena>, y: &Ref<'arena>) -> bool {
        ptr::eq::<Node>(*x, *y)
    }

    fn elem_name<'a>(&self, target: &'a Ref<'arena>) -> ExpandedName<'a> {
        match target.data {
            NodeData::Element { ref name, .. } => name.expanded(),
            _ => panic!("not an element!"),
        }
    }

    fn get_template_contents(&mut self, target: &Ref<'arena>) -> Ref<'arena> {
        if let NodeData::Element {
            template_contents: Some(contents),
            ..
        } = target.data
        {
            contents
        } else {
            panic!("not a template element!")
        }
    }

    fn is_mathml_annotation_xml_integration_point(&self, target: &Ref<'arena>) -> bool {
        if let NodeData::Element {
            mathml_annotation_xml_integration_point,
            ..
        } = target.data
        {
            mathml_annotation_xml_integration_point
        } else {
            panic!("not an element!")
        }
    }

    fn create_element(
        &mut self,
        name: QualName,
        attrs: Vec<Attribute>,
        flags: ElementFlags,
    ) -> Ref<'arena> {
        self.new_node(NodeData::Element {
            name,
            attrs: RefCell::new(attrs),
            template_contents: if flags.template {
                Some(self.new_node(NodeData::Document))
            } else {
                None
            },
            mathml_annotation_xml_integration_point: flags.mathml_annotation_xml_integration_point,
        })
    }

    fn create_comment(&mut self, text: StrTendril) -> Ref<'arena> {
        self.new_node(NodeData::Comment { contents: text })
    }

    fn create_pi(&mut self, target: StrTendril, data: StrTendril) -> Ref<'arena> {
        self.new_node(NodeData::ProcessingInstruction {
            target,
            contents: data,
        })
    }

    fn append(&mut self, parent: &Ref<'arena>, child: NodeOrText<Ref<'arena>>) {
        self.append_common(
            child,
            || parent.last_child.get(),
            |new_node| parent.append(new_node),
        )
    }

    fn append_before_sibling(&mut self, sibling: &Ref<'arena>, child: NodeOrText<Ref<'arena>>) {
        self.append_common(
            child,
            || sibling.previous_sibling.get(),
            |new_node| sibling.insert_before(new_node),
        )
    }

    fn append_based_on_parent_node(
        &mut self,
        element: &Ref<'arena>,
        prev_element: &Ref<'arena>,
        child: NodeOrText<Ref<'arena>>,
    ) {
        if element.parent.get().is_some() {
            self.append_before_sibling(element, child)
        } else {
            self.append(prev_element, child)
        }
    }

    fn append_doctype_to_document(
        &mut self,
        name: StrTendril,
        _public_id: StrTendril,
        _system_id: StrTendril,
    ) {
        self.document
            .append(self.new_node(NodeData::Doctype { name }))
    }

    fn add_attrs_if_missing(&mut self, target: &Ref<'arena>, attrs: Vec<Attribute>) {
        let mut existing = if let NodeData::Element { ref attrs, .. } = target.data {
            attrs.borrow_mut()
        } else {
            panic!("not an element")
        };

        let existing_names = existing
            .iter()
            .map(|e| e.name.clone())
            .collect::<HashSet<_>>();
        existing.extend(
            attrs
                .into_iter()
                .filter(|attr| !existing_names.contains(&attr.name)),
        );
    }

    fn remove_from_parent(&mut self, target: &Ref<'arena>) {
        target.detach()
    }

    fn reparent_children(&mut self, node: &Ref<'arena>, new_parent: &Ref<'arena>) {
        let mut next_child = node.first_child.get();
        while let Some(child) = next_child {
            debug_assert!(ptr::eq::<Node>(child.parent.get().unwrap(), *node));
            next_child = child.next_sibling.get();
            new_parent.append(child)
        }
    }
}

// }

// ////////////////////////////////////////////////////////////////////////////////////////////////

// Copied and adapted from
// https://github.com/servo/html5ever/blob/cfea19f74d922ad049614cdb88a7805bc4ea7e06/rcdom/lib.rs
// {

// Copyright 2014-2017 The html5ever Project Developers. See the
// COPYRIGHT file at the top-level directory of this distribution.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

enum SerializeOp<'arena> {
    Open(Ref<'arena>),
    Close(QualName),
}

impl<'arena> Serialize for Ref<'arena> {
    fn serialize<S>(&self, serializer: &mut S, traversal_scope: TraversalScope) -> io::Result<()>
    where
        S: Serializer,
    {
        let mut ops = VecDeque::new();
        match traversal_scope {
            TraversalScope::IncludeNode => ops.push_back(SerializeOp::Open(self)),
            TraversalScope::ChildrenOnly(_) => {
                let mut child_iter = self.first_child.get();
                while let Some(child) = child_iter {
                    child_iter = child.next_sibling.get();
                    ops.push_back(SerializeOp::Open(child));
                }
            }
        }

        while let Some(op) = ops.pop_front() {
            match op {
                SerializeOp::Open(handle) => match handle.data {
                    NodeData::Element {
                        ref name,
                        ref attrs,
                        ..
                    } => {
                        serializer.start_elem(
                            name.clone(),
                            attrs.borrow().iter().map(|at| (&at.name, &at.value[..])),
                        )?;

                        ops.push_front(SerializeOp::Close(name.clone()));

                        let mut child_iter = handle.last_child.get();
                        while let Some(child) = child_iter {
                            child_iter = child.previous_sibling.get();
                            ops.push_front(SerializeOp::Open(child));
                        }
                    }

                    NodeData::Doctype { ref name, .. } => serializer.write_doctype(name)?,

                    NodeData::Text { ref contents } => serializer.write_text(&contents.borrow())?,

                    NodeData::Comment { ref contents } => serializer.write_comment(contents)?,

                    NodeData::ProcessingInstruction {
                        ref target,
                        ref contents,
                    } => serializer.write_processing_instruction(target, contents)?,

                    NodeData::Document => {
                        return Err(io::Error::new(
                            io::ErrorKind::Other,
                            "Can't serialize Document node itself",
                        ))
                    }
                },

                SerializeOp::Close(name) => {
                    serializer.end_elem(name)?;
                }
            }
        }

        Ok(())
    }
}

// }

/// A wrapper for [`Parser<ArenaSink<'_>>`](Parser) that implements [fmt::Write](fmt::Write)
/// and [io::Write](io::Write).
pub struct ArenaSinkParser<'arena>(Parser<ArenaSink<'arena>>);

impl fmt::Write for ArenaSinkParser<'_> {
    #[inline]
    fn write_str(&mut self, s: &str) -> fmt::Result {
        self.0.process(s.into());
        Ok(())
    }
}

struct ArenaIoAdaptor<'a, 'arena>(&'a mut ArenaSinkParser<'arena>);

impl fmt::Write for ArenaIoAdaptor<'_, '_> {
    #[inline]
    fn write_str(&mut self, s: &str) -> fmt::Result {
        self.0.write_str(s)
    }
}

impl<'arena> io::Write for ArenaSinkParser<'arena> {
    #[inline]
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.write_all(buf)?;
        Ok(buf.len())
    }

    #[inline]
    fn flush(&mut self) -> io::Result<()> {
        Ok(())
    }

    #[inline]
    fn write_all(&mut self, buf: &[u8]) -> io::Result<()> {
        let s =
            std::str::from_utf8(buf).map_err(|err| io::Error::new(io::ErrorKind::Other, err))?;
        self.0.process(s.into());
        Ok(())
    }

    #[inline]
    fn write_fmt(&mut self, fmt: fmt::Arguments<'_>) -> io::Result<()> {
        match fmt::write(&mut ArenaIoAdaptor(self), fmt) {
            Ok(()) => Ok(()),
            Err(..) => Err(io::Error::new(io::ErrorKind::Other, "formatter error")),
        }
    }
}

impl<'arena> ArenaSinkParser<'arena> {
    /// Build a new ArenaSinkParser.
    ///
    /// # Arguments
    ///
    /// * `arena` - The allocator to use.
    /// * `opts` - Options to pass to `parse_document()`.
    #[inline]
    pub fn new(arena: Arena<'arena>, opts: ParseOpts) -> Self {
        Self(parse_document(ArenaSink::new(arena), opts))
    }

    /// Finish rendering and return the document node.
    #[inline]
    pub fn finish(self) -> Ref<'arena> {
        self.0.finish()
    }
}

/// Render a template into a writer, e.g. a [`Vec<u8>`].
///
/// This is an oppinionated default implementation that disables [html5ever]'s script rendering.
pub fn render(tmpl: &impl fmt::Display, dest: impl io::Write) -> Result<(), io::Error> {
    // render
    let arena = typed_arena::Arena::new();
    let mut parser = ArenaSinkParser::new(
        &arena,
        ParseOpts {
            tree_builder: TreeBuilderOpts {
                scripting_enabled: false,
                ..TreeBuilderOpts::default()
            },
            ..ParseOpts::default()
        },
    );
    write!(parser, "{}", tmpl)?;
    let document = parser.finish();

    // serialize
    let mut serializer = HtmlSerializer::new(
        dest,
        SerializeOpts {
            scripting_enabled: false,
            create_missing_parent: true,
            ..SerializeOpts::default()
        },
    );
    document.serialize(&mut serializer, TraversalScope::ChildrenOnly(None))?;

    Ok(())
}
