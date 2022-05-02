import { ChangeEvent, useEffect, useState } from "react";
import Bookable from "../../domain/Bookable";

export interface FormState {
  state: Bookable | undefined;

  handleChange: (
    _: ChangeEvent<HTMLInputElement | HTMLSelectElement | HTMLTextAreaElement>
  ) => void;
  handleChecked: (_: ChangeEvent<HTMLInputElement>) => void;
}

export default function useFormState(data?: Bookable): FormState {
  const [state, setState] = useState(data);

  useEffect(() => {
    if (data) {
      setState(data);
    }
  }, [data]);

  function handleChange(
    e: ChangeEvent<HTMLInputElement | HTMLSelectElement | HTMLTextAreaElement>
  ) {
    setState({
      ...(state || ({} as Bookable)),
      [e.target.name]: e.target.value,
    });
  }

  function handleChecked(e: ChangeEvent<HTMLInputElement>) {
    const { name, value, checked } = e.target;
    const stateValues = (state || {})[name as "days" | "sessions"];
    const values = new Set(stateValues);
    const intValue = parseInt(value, 10);

    values.delete(intValue);
    if (checked) values.add(intValue);

    setState({
      ...(state || ({} as Bookable)),
      [name]: Array.from(values),
    });
  }

  return {
    state,
    handleChange,
    handleChecked,
  };
}
