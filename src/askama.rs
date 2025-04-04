use std::fmt;

use askama::Template;
use askama::filters::FastWritable;

use crate::{ArenaSinkParser, default_parse_opts, serialize};

pub struct TidyTemplate<T>(pub T);

impl<T: Template> Template for TidyTemplate<T> {
    fn render_into_with_values<W: core::fmt::Write + ?Sized>(
        &self,
        writer: &mut W,
        values: &dyn askama::Values,
    ) -> askama::Result<()> {
        let arena = typed_arena::Arena::new();
        let mut parser = ArenaSinkParser::new(&arena, default_parse_opts());
        self.0.render_into_with_values(&mut parser, values)?;
        serialize(writer, parser.finish())?;
        Ok(())
    }

    const SIZE_HINT: usize = T::SIZE_HINT;
}

impl<T: Template> fmt::Display for TidyTemplate<T> {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.render_into(f).map_err(|_| fmt::Error)
    }
}

impl<T: Template> FastWritable for TidyTemplate<T> {
    #[inline]
    fn write_into<W: core::fmt::Write + ?Sized>(&self, dest: &mut W) -> askama::Result<()> {
        self.render_into(dest)
    }
}

/// Extension trait to wrap a [`Template`] in [`TidyTemplate`].
///
/// ```
/// use askama::Template;
/// use html5ever_arena_dom::TidyTemplateExt;
///
/// #[derive(Debug, Template)]
/// #[template(
///     ext = "html",
///     source = "<title>Test</title><h1>Hello, <b>{{user}}</strong>!</h2"
/// )]
/// struct Hello<'a> {
///     user: &'a str,
/// }
///
/// assert_eq!(
///     Hello { user: "wörld" }.as_tidy().render().unwrap(),
///     "<html><head><title>Test</title></head>\
///      <body><h1>Hello, <b>wörld!</b></h1></body></html>"
/// );
/// ```
pub trait TidyTemplateExt: Template {
    /// Wrap a reference to the template.
    fn as_tidy(&self) -> TidyTemplate<&Self>;

    /// Wrap the template.
    fn into_tidy(self) -> TidyTemplate<Self>
    where
        Self: Sized;
}

impl<T: Template> TidyTemplateExt for T {
    #[inline]
    fn as_tidy(&self) -> TidyTemplate<&Self> {
        TidyTemplate(self)
    }

    #[inline]
    fn into_tidy(self) -> TidyTemplate<Self>
    where
        Self: Sized,
    {
        TidyTemplate(self)
    }
}
