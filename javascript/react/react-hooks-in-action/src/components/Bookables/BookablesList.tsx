import { ChangeEvent, useEffect, useRef } from "react";
import { FaArrowRight, FaSpinner } from "react-icons/fa";
import Bookable from "../../domain/Bookable";
import { getData } from "../../utils/api";
import { BookableAction, BookableActionType, BookablesState } from "./reducer";

export interface BookablesListProps {
  state: BookablesState;
  dispatch: React.Dispatch<BookableAction>;
}

export default function BookablesList({ state, dispatch }: BookablesListProps) {
  const { group, bookableIndex, isLoading, error, bookables } = state;

  const bookablesInGroup = bookables.filter((b) => b.group === group);
  const groups = Array.from(new Set(bookables.map((b) => b.group)));

  const nextButtonRef = useRef<HTMLButtonElement>(null);

  useEffect(() => {
    dispatch({ type: BookableActionType.FetchBookablesRequest });
    getData<Bookable[]>("http://localhost:3001/bookables")
      .then((bookables) =>
        dispatch({
          type: BookableActionType.FetchBookablesSuccess,
          payload: bookables,
        })
      )
      .catch((error: Error) =>
        dispatch({
          type: BookableActionType.FetchBookablesError,
          payload: error,
        })
      );
  }, [dispatch]);

  if (error) {
    return <p>{error.message}</p>;
  }

  if (isLoading) {
    return (
      <p>
        <FaSpinner /> Loading bookables...
      </p>
    );
  }

  function changeGroup(event: ChangeEvent<HTMLSelectElement>) {
    dispatch({
      type: BookableActionType.SetGroup,
      payload: event.target.value,
    });
  }

  function changeBookable(selectedIndex: number) {
    dispatch({
      type: BookableActionType.SetBookable,
      payload: selectedIndex,
    });
    nextButtonRef?.current?.focus();
  }

  function nextBookable() {
    dispatch({
      type: BookableActionType.NextBookable,
    });
  }

  return (
    <div>
      <select value={group} onChange={changeGroup}>
        {groups.map((g) => (
          <option value={g} key={g}>
            {g}
          </option>
        ))}
      </select>
      <ul className="bookables items-list-nav">
        {bookablesInGroup.map((b, i) => (
          <li
            key={b.id}
            className={i === bookableIndex ? "selected" : undefined}
          >
            <button className="btn" onClick={() => changeBookable(i)}>
              {b.title}
            </button>
          </li>
        ))}
      </ul>
      <p>
        <button
          className="btn"
          onClick={nextBookable}
          autoFocus
          ref={nextButtonRef}
        >
          <FaArrowRight />
          <span>Next</span>
        </button>
      </p>
    </div>
  );
}
