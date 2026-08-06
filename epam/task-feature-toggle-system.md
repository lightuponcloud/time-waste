# Feature Toggle System

Feature Toggles (also known as Feature Flags) are used to enable or disable application functionality without deploying new code.

Given a list of feature configuration records and a list of queries, determine whether each queried feature is enabled in the requested environment.

## Configuration Format

A single configuration string has the following format:

“<feature_name> <environment> <status>”
where:

- `feature_name` is the name of the feature
- `environment` is the target environment (for example: `dev`, `staging`, `prod`)
- `status` is either `enabled` or `disabled`

### Rules

- If the same feature/environment combination appears multiple times, the **most recent** valid configuration overrides all previous ones.
- Invalid configuration records must be ignored (they should not cause errors).
- Feature names and environments contain no spaces.

## Queries

Each query is a pair `(feature_name, environment)`.

For each query, return:

- `True` — if the feature is **enabled** in the specified environment
- `False` — if the feature is **disabled** or if no valid configuration exists for that feature/environment combination

## Function Signature

```python
def feature_toggles(configs: list[str], queries: list[tuple[str, str]]) -> list[bool]:
...


Examples
Example 1
Input:

configs = [
"search prod enabled",
"search dev disabled"
]

queries = [
("search", "prod"),
("search", "dev"),
("search", "staging")
]

Output:

[True, False, False]


Explanation:
• search is enabled in prod
• search is disabled in dev
• No configuration exists for search in staging → False


Example 2
Input:

configs = [
"checkout prod disabled",
"checkout prod enabled"
]

queries = [
("checkout", "prod")
]


Output:

[True]

Explanation: The last valid configuration for checkout / prod is enabled, so it overrides the previous disabled status.


Example 3
Input:

configs = [
"invalid record",
"recommendations prod enabled"
]

queries = [
("recommendations", "prod")
]


Output: [True]

Explanation: The invalid record is ignored. The remaining valid configuration enables the feature.
Constraints
• 1 ≤ len(configs) ≤ 10⁵
• 1 ≤ len(queries) ≤ 10⁵
• Configuration entries are strings
• Feature names and environments contain no spaces
• Invalid configuration records must be ignored and must not raise errors
• When multiple valid configurations exist for the same feature/environment pair, the last one takes precedence
Notes
• The system will check the correctness of the solution, not the performance.
• A configuration is considered valid only if it consists of exactly three space-separated parts and the status is either "enabled" or "disabled".

