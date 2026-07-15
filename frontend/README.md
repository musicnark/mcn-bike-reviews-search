# Frontend

This is a ClojureScript app built with Shadow CLJS and Reagent. Reagent renders through React, which keeps the implementation aligned with the Clojure backend while still using a mainstream browser UI runtime.

## Local Development

Run the API from the repository root:

```sh
clojure -M:api
```

Run the frontend from this directory:

```sh
npm run dev
```

The frontend dev server runs at `http://localhost:8020` and calls the API at
`http://localhost:3000/api`.

## Architecture Constraints

Write the frontend with React maintainers in mind:

- Use plain Reagent function components with obvious names.
- Keep component files named by feature or UI role, such as `layout`, `search`,
  `filters`, and `results`.
- Pass data into components with simple props-style maps.
- Keep API calls and other side effects out of view components.
- Use Reagent atoms sparingly and keep shared state easy to trace.
- Avoid macros, framework magic, and clever abstractions unless they clearly
  reduce real complexity.
- Prioritise accessible controls, semantic HTML, keyboard support, and clear
  focus states.
- Keep the UI polished, responsive, and understandable to someone familiar with
  React even if they do not know ClojureScript.

The first implementation milestone is a small API-backed shell: mount the app,
fetch `/api/health` and `/api/fields`, and display the available search fields.
