import { useReducer } from "react";
import BookablesList from "./BookablesList";
import BookableDetails from "./BookableDetails";
import reducer, { initialState } from "./reducer";

export default function BookablesView() {
  const [state, dispatch] = useReducer(reducer, initialState);

  const bookablesInGroup = state.bookables.filter(
    (b) => b.group === state.group
  );
  const bookable = bookablesInGroup[state.bookableIndex];

  return (
    <>
      <BookablesList state={state} dispatch={dispatch} />
      <BookableDetails bookable={bookable} />
    </>
  );
}
