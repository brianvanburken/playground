import { ChangeEvent, useReducer } from "react";
import { FaArrowRight } from "react-icons/fa";
import { bookables, days, sessions } from "../../static.json";
import reducer, { BookableActionType, BookablesState } from "./reducer";

const initialState: BookablesState = {
  group: "Rooms",
  bookableIndex: 0,
  hasDetails: true,
  bookables,
};

export default function BookablesList() {
  const [state, dispatch] = useReducer(reducer, initialState);
  const { group, bookableIndex, hasDetails } = state;

  const bookablesInGroup = bookables.filter((b) => b.group === group);
  const groups = Array.from(new Set(bookables.map((b) => b.group)));
  const bookable = bookablesInGroup[bookableIndex];

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
          <button className="btn" onClick={nextBookable} autoFocus>
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
