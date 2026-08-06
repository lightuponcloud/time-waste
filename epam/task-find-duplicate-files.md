# Find Duplicate File in System

## Problem

Given a list `paths` of directory info, including the directory path and all the files with contents in this directory, return all the duplicate files in the file system **in terms of their paths**. You may return the answer in any order.

A group of duplicate files consists of **at least two files** that have the **same content**.

A single directory info string in the input list has the following format:

```
"root/d1/d2/.../dm f1.txt(f1_content) f2.txt(f2_content) ... fn.txt(fn_content)"
```

It means there are `n` files (`f1.txt`, `f2.txt` ... `fn.txt`) with content (`f1_content`, `f2_content` ... `fn_content`) respectively in the directory `"root/d1/d2/.../dm"`.

**Notes:**
- `n >= 1` and `m >= 0`
- If `m = 0`, it means the directory is just the root directory.
- A single blank space separates the directory path and file info.
- You may assume no files or directories share the same name in the same directory.
- You may assume each given directory info represents a unique directory.

The output is a list of groups of duplicate file paths. For each group, it contains all the file paths of the files that have the same content. A file path is a string that has the following format:

```
"directory_path/file_name"
```

---

## Example 1

**Input:**
```python
paths = [
    "root/a 1.txt(abcd) 2.txt(efgh)",
    "root/c 3.txt(abcd)",
    "root/c/d 4.txt(efgh)",
    "root 4.txt(efgh)"
]
```

**Output:**
```python
[
    ["root/a/2.txt", "root/c/d/4.txt", "root/4.txt"],
    ["root/a/1.txt", "root/c/3.txt"]
]
```

---

## Example 2

**Input:**
```python
paths = [
    "root/a 1.txt(abcd) 2.txt(efgh)",
    "root/c 3.txt(abcd)",
    "root/c/d 4.txt(efgh)"
]
```

**Output:**
```python
[
    ["root/a/2.txt", "root/c/d/4.txt"],
    ["root/a/1.txt", "root/c/3.txt"]
]
```

---

## Constraints

- `1 <= paths.length <= 2 * 10^4`
- `1 <= paths[i].length <= 3000`
- `1 <= sum(paths[i].length) <= 5 * 10^5`
- `paths[i]` consists of English letters, digits, `'/'`, `'.'`, `'('`, `')'`, and spaces.
- The system will check the **correctness** of the solution, not the performance.

---

## Solution (Python)

```python
from collections import defaultdict
from typing import List

class Solution:
    def findDuplicate(self, paths: List[str]) -> List[List[str]]:
        content_to_paths = defaultdict(list)
        
        for path_info in paths:
            parts = path_info.split()
            directory = parts[0]
            
            for file_info in parts[1:]:
                # file_info format: "filename.txt(content)"
                left_paren = file_info.index('(')
                filename = file_info[:left_paren]
                content = file_info[left_paren + 1 : -1]  # remove surrounding ()
                
                full_path = f"{directory}/{filename}"
                content_to_paths[content].append(full_path)
        
        # Keep only groups that contain at least 2 files
        return [group for group in content_to_paths.values() if len(group) >= 2]
```

---

## Explanation

1. Use a dictionary that maps **content → list of full file paths**.
2. For each directory info string:
   - Split on spaces → the first token is the directory path.
   - The remaining tokens are of the form `"filename(content)"`.
   - Extract the filename and the content, then build the absolute path `directory/filename`.
3. After processing all inputs, return only the lists that contain **two or more** paths (i.e., actual duplicates).

This solution correctly groups files by their content and filters out unique files.
