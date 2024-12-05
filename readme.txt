Names: Satyam Patel, Emmanuel Usman

Description:
This code parses natural language commands into a structured query format and then applies these queries to a Prolog knowledge base (facts.pl). 
Using a DCG grammar, it recognizes commands that resemble SQL queries and supports operations like joins, matches, and where conditions.
After parsing, it filters the data according to the query’s criteria and outputs the results in a tabular format.

Function:
- Parses input commands (e.g., "Get all from employee") into an internal query representation.
- Evaluates queries against provided facts to filter and return matching rows.
- Handles conditions such as `is less than`, `is greater than`, and `equals` for both numeric, string, and date values.
- Prints the filtered results as tables.

Comments/Bugs/Self-Assessment:
- All basic requirements are met: parsing, evaluating conditions, and printing tables.
- No known bugs at this time.
- The code has been tested with the given example commands and produces expected outputs.
