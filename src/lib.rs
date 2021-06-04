// Copyright 2021 René Kijewski and the html5ever Project Developers.
// See the COPYRIGHT file at the top-level directory of this distribution.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use std::borrow::Cow;
use std::cell::{Cell, RefCell};
use std::collections::{HashSet, VecDeque};
use std::{fmt, io, ptr};

use askama::Template;
use html5ever::driver::{parse_document, ParseOpts};
use html5ever::interface::tree_builder::{ElementFlags, NodeOrText, QuirksMode, TreeSink};
use html5ever::serialize::{HtmlSerializer, Serialize, SerializeOpts, Serializer, TraversalScope};
use html5ever::tendril::{StrTendril, TendrilSink as _};
use html5ever::tree_builder::TreeBuilderOpts;
use html5ever::{Attribute, ExpandedName, Parser, QualName};
use thiserror::Error;

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

pub type Arena<'arena> = &'arena typed_arena::Arena<Node<'arena>>;

pub type Ref<'arena> = &'arena Node<'arena>;

type Link<'arena> = Cell<Option<Ref<'arena>>>;

pub struct ArenaSink<'arena> {
    arena: Arena<'arena>,
    document: Ref<'arena>,
}

pub struct Node<'arena> {
    parent: Link<'arena>,
    next_sibling: Link<'arena>,
    previous_sibling: Link<'arena>,
    first_child: Link<'arena>,
    last_child: Link<'arena>,
    data: NodeData<'arena>,
}

pub enum NodeData<'arena> {
    Document,
    Doctype {
        name: StrTendril,
        public_id: StrTendril,
        system_id: StrTendril,
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
            template_contents: Some(ref contents),
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
        public_id: StrTendril,
        system_id: StrTendril,
    ) {
        self.document.append(self.new_node(NodeData::Doctype {
            name,
            public_id,
            system_id,
        }))
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

                    NodeData::Doctype { ref name, .. } => serializer.write_doctype(&name)?,

                    NodeData::Text { ref contents } => serializer.write_text(&contents.borrow())?,

                    NodeData::Comment { ref contents } => serializer.write_comment(&contents)?,

                    NodeData::ProcessingInstruction {
                        ref target,
                        ref contents,
                    } => serializer.write_processing_instruction(target, contents)?,

                    NodeData::Document => panic!("Can't serialize Document node itself"),
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

struct ArenaSinkParser<'arena> {
    inner: Parser<ArenaSink<'arena>>,
}

impl fmt::Write for ArenaSinkParser<'_> {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        self.inner.process(s.into());
        Ok(())
    }
}

impl<'arena> ArenaSinkParser<'arena> {
    pub fn new(arena: Arena<'arena>, opts: ParseOpts) -> Self {
        let inner = parse_document(ArenaSink::new(arena), opts);
        Self { inner }
    }

    pub fn finish(self) -> Ref<'arena> {
        self.inner.finish()
    }
}

#[derive(Debug, Error)]
pub enum RenderError {
    #[error("A rendering error occured: {0}")]
    Render(askama::Error),

    #[error("A serialization error occured: {0}")]
    Serialize(io::Error),
}

pub fn render(
    arena: Arena<'_>,
    tmpl: &impl Template,
    dest: impl io::Write,
) -> Result<(), RenderError> {
    // render
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
    tmpl.render_into(&mut parser)
        .map_err(RenderError::Render)?;
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
    document
        .serialize(&mut serializer, TraversalScope::ChildrenOnly(None))
        .map_err(RenderError::Serialize)?;

    Ok(())
}

#[cfg(test)]
mod tests {
    use askama::Template;

    #[test]
    fn test_render_simple_invocation() {
        #[derive(Template)]
        #[template(ext = "html", source = "<EM>Test&{{index}}>!")]
        struct TestTemplate {
            index: u32,
        }

        let arena = typed_arena::Arena::new();
        let mut data = Vec::default();
        super::render(&arena, &TestTemplate { index: 1 }, &mut data).expect("Rendered ok");
        let data = std::str::from_utf8(&data).expect("Valid UTF-8");

        assert_eq!(
            data,
            "<html><head></head><body><em>Test&amp;1&gt;!</em></body></html>"
        );
    }
}
