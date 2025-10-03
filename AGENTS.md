# Agent Instructions for OpenLDK Development

## Linting

Lint this code with `ocicl lint openldk.asd`.

**Important guidelines:**
- Lint frequently as you fix problems to ensure you aren't introducing new issues
- The linter must not find any problems before committing
- Run `make` periodically to verify the build still works
- Address all linting errors and warnings before finalizing changes

## Commit Practices

Commit frequently with well-structured, multi-line commit messages:

```
Short summary of change (50 chars or less)

Detailed explanation of what was changed and why. Use proper
line wrapping at 72 characters. This is NOT just inserting \n
characters into commit strings, but properly formatted text.

- Use bullet points if listing multiple changes
- Reference issue numbers if applicable
- Explain the rationale behind non-obvious changes
```

## Development Workflow

1. **Before making changes:**
   - Understand the affected components and architecture
   - Identify which files need modification

2. **While making changes:**
   - Lint after each significant change
   - Run `make` to verify compilation
   - Test relevant functionality

3. **Before committing:**
   - Final lint check with `ocicl lint openldk.asd`
   - Verify `make` succeeds
   - Ensure all tests still pass (if applicable)
   - Write a descriptive multi-line commit message

## Code Style

Follow Common Lisp conventions and the existing code style in the repository. The linter will catch most style issues.

