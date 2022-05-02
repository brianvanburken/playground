```python
if (light < 0.01) != (temperature > 0.0):
    turn_camera_on()
```

In this case if one is `True` and the other `False` the statement evaluates to
`True`, since `True != False`. But, if both are `True` the statement will evaluate
to `False`, since they cannot be equal. You want either of them to be true so
you must use a `xor` operator.