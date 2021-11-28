import { ChangeEvent, useEffect, useReducer, useRef } from "react";
import { FaArrowRight, FaSpinner } from "react-icons/fa";
import Bookable from "../../domain/Bookable";
import { days, sessions } from "../../static.json";
import { getData } from "../../utils/api";
import reducer, { BookableActionType, initialState } from "./reducer";

export default function BookablesList() {
  const [state, dispatch] = useReducer(reducer, initialState);
  const { group, bookableIndex, hasDetails, isLoading, error, bookables } =
    state;

  const bookablesInGroup = bookables.filter((b) => b.group === group);
  const groups = Array.from(new Set(bookables.map((b) => b.group)));
  const bookable = bookablesInGroup[bookableIndex];

  const timerRef = useRef<number>();
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
  }, []);

  useEffect(() => {
    timerRef.current = window.setInterval(() => {
      dispatch({ type: BookableActionType.NextBookable });
    }, 3000);

    return stopPresentation;
  }, []);

  function stopPresentation() {
    window.clearInterval(timerRef.current);
  }

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

  function toggleDetails() {
    dispatch({
      type: BookableActionType.ToggleHasDetails,
    });
  }

  return (
    <>
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

      {bookable && (
        <div className="bookable-details">
          <div className="item">
            <div className="item-header">
              <h2>{bookable.title}</h2>
              <span className="controls">
                <label>
                  <input
                    type="checkbox"
                    checked={hasDetails}
                    onChange={toggleDetails}
                  />
                  Show Details
                </label>
                <button className="btn" onClick={stopPresentation}>
                  Stop
                </button>
              </span>
            </div>

            <p>{bookable.notes}</p>

            {hasDetails && (
              <div className="item-details">
                <h3>Availability</h3>
                <div className="bookable-availability">
                  <ul>
                    {bookable.days.sort().map((d) => (
                      <li key={d}>{days[d]}</li>
                    ))}
                  </ul>
                  <ul>
                    {bookable.sessions.map((s) => (
                      <li key={s}>{sessions[s]}</li>
                    ))}
                  </ul>
                </div>
              </div>
            )}
          </div>
        </div>
      )}
    </>
  );
}
