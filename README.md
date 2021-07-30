A simple library to parse [askama](https://djc.github.io/askama/) templates,
and tidy them up using [html5ever](https://doc.servo.org/html5ever/index.html).

This library simply extracts and combines two usage examples of html5ever, and makes them re-usable.
It is mostly meant to be used with template engines such as [Askama](https://crates.io/crates/askama) or
[nate](https://crates.io/crates/nate), which use fmt::Write to output their generated HTML data.
