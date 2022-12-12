# Distinction of Use Cases

## Scoring / Rating [Scope: Search]

Currently it is not planned to support scoring or rating of snippets. That means it is not possible to retrieve the most viewed, best rated or most often downloaded snippets.

## Batch Manipulation

It is not planned to support batch manipulation, i.e. editing of several snippets at once or similar actions. If a feature like this will be implemented at some point, then as frontend only feature, resulting in multiple API requests.

## Snippet Conversion

A feature to convert snippets in between formats will be implemented, although it will not support intermediate transformation via script languages like lua/python.

## Invalidation of Sessions / Logout

It is not possible to invalidate sessions. This means that although the logout on the frontend is possible, the underlying session would still be active. A stateless server may not store any state information, therefore the session token has to be stored client side. The server has to accept all tokens it gave out for a certain period of time. This is also a known MITM vulnerability that is impossible to fix.
