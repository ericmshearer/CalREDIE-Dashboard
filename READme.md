# Generic UDF Dashboard

**Last Updated:** 10/04/2023<br> **Built with:** R Version 4.3.1

## Overview

Shiny App intended for epidemiologists working in California at the
county level. Features included in this build are basic but can be
expanded by adding maps (static/interactive), download buttons, and
other filtering options.

<img src="www/layout.png" width="843"/>

## How it Works

Make a copy of the repository and publish to your local instance of
Posit Connect. App will run without Posit Connect, but sharing options
will be limited.

Shiny app is limited to ingesting Complete UDF Exports from the CalREDIE
Data Distribution Portal. Begin by uploading the file to the app, then
select your inclusion criteria and years for further filtering. All case
counts are aggregated at the calendar year and month level, which may
deviate from other reporting methods.

Any feedback or questions about modifications:
[ericshearer\@me.com](mailto:ericshearer@me.com){.email}
