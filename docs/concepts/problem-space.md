# Motorcycle News

MCN is a weekly print magazine and website owned by Bauer Media. They generate revenue by monetising attention and trust with this content. The Bike Reviews section is one of the biggest content avenues when measured by page views, making any workflow improvements to this area lucrative.

## MCN's Tech Stack

MCN's website at the time was based on WordPress, and the team uploading content to it had some technical literacy. They had been promised a new CMS by the company, that had been in the works for over twelve months, and rollout was being repeatedly delayed. Nobody on the editorial team had access to the source code for the website, including me, just the content on it. They used Monday.com to organise workflows.

## This Project's Tech Stack

The prototype version of this tool was written in Elisp, as it was a language I was very familiar with at the time. I had written many automation tools to aid my own editorial workflows, and the proof of concept didn't need any of the features of a production-ready language.

Clojure was used for the SLC version of this tool because:
- Clojure's exploratory REPL-based workflows, and verbose yet concise code, ultimately mean the problem is central, not the language demands. This workflow hugely facilitated my learning around the problem space while still being productive.
- Clojure is a highly stable language with a large repository of stable libraries, which is ideal for any back-end service.
- Clojure's functional programming principles align well with the needs of this project; particularly with regards to composition, controlled state, declarative collection processing, explicit error handling capabilities, and data oriented design.
- Clojure is based on the JVM, making hosting and deployment rather simple.
