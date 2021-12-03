import { ChangeEvent, useState } from "react";

export default function App() {
  return (
    <>
      <StateField />
    </>
  );
}

function StateField() {
  const [value, setValue] = useState<string>("");
  const [errors, setErrors] = useState<Record<string, string>>({});

  function handleChange(event: ChangeEvent<HTMLInputElement>) {
    event.preventDefault();

    const val = event.target.value;

    const validationErrors: Record<string, string> = {};

    if (!new RegExp(/^hello\s.*?$/).test(val)) {
      validationErrors["pattern"] =
        'Must start with "hello" and followed by anything';
    }

    setValue(val);
    setErrors(validationErrors);
  }

  return (
    <>
      <input type="text" onChange={handleChange} value={value} />
      {errors ? (
        <>{errors.pattern ? <span>{errors.pattern}</span> : null}</>
      ) : null}
    </>
  );
}
