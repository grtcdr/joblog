# `2024_12_08.db5b033`

- Added support for overlays and a new variable `joblog-mark-old-entries` used to toggle this feature
- Fixed edge case handling for inserting entries with older dates
- Fix the parser to correctly highlight the status element

# `2024_12_04.7839ec7`

- Improve the precision of the parser
- Fixed duplicate handling

# `2024_12_03.76bca07`

- Add support for entering older dates whilst respecting the chronological nature of the format
- Refactor parser to utilize regexp groups making the codebase leaner and easier to reason about
- Remove duplicate entries from search results

# `2024_12_02.ba9b99d`

This marks the first release which includes basic job logging capabilities.
