```python
ph = float(input("Enter the ph level: "))
if ph < 7.0:
    print("It's acidic!")
elif ph < 4.0:
    print("It's a strong acid!")
```

Input: 6.4
Output: "It's acidic!"

Input: 3.6
Output: "It's acidic!"

Changes:

```python
ph = float(input("Enter the ph level: "))
if ph < 7.0:
    print("It's acidic!")
if ph < 4.0:
    print("It's a strong acid!")
```