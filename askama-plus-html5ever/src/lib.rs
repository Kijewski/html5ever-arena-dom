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

//! A simple library to parse [askama] templates, and tidy them up using
//! [html5ever](https://doc.servo.org/html5ever/index.html).

use std::{fmt, io};

use askama::Template;

struct DisplayAdaptor<'a, T: Template>(&'a T);

impl<T: Template> fmt::Display for DisplayAdaptor<'_, T> {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.render_into(f).map_err(|_| fmt::Error)
    }
}

/// Render a template into a writer, e.g. a [`Vec<u8>`].
///
/// This is an oppinionated default implementation that disables
/// [html5ever](https://doc.servo.org/html5ever/index.html)'s script rendering.
#[inline]
pub fn render(tmpl: &impl Template, dest: impl io::Write) -> Result<(), io::Error> {
    html5ever_arena_dom::render(&DisplayAdaptor(tmpl), dest)
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

        let mut data = Vec::default();
        super::render(&TestTemplate { index: 1 }, &mut data).expect("Rendered ok");
        let data = std::str::from_utf8(&data).expect("Valid UTF-8");

        assert_eq!(
            data,
            "<html><head></head><body><em>Test&amp;1&gt;!</em></body></html>"
        );
    }
}
