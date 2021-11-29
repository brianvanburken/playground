import { Dispatch, useState } from "react";
import {
  FaCalendarCheck,
  FaCalendarDay,
  FaChevronLeft,
  FaChevronRight,
} from "react-icons/fa";
import { WeekAction, WeekActionType } from "./weekReducer";

export interface WeekPickerProps {
  dispatch: Dispatch<WeekAction>;
}

export default function WeekPicker({ dispatch }: WeekPickerProps) {
  const [dateText, setDateText] = useState("2020-06-24");

  function goToDate() {
    dispatch({
      type: WeekActionType.SetDate,
      payload: dateText,
    });
  }

  return (
    <div>
      <p className="date-picker">
        <button
          className="btn"
          onClick={() => dispatch({ type: WeekActionType.PrevWeek })}
        >
          <FaChevronLeft />
          <span>Prev</span>
        </button>
        <button
          className="btn"
          onClick={() => dispatch({ type: WeekActionType.Today })}
        >
          <FaCalendarDay />
          <span>Today</span>
        </button>

        <span>
          <input
            type="text"
            placeholder="e.g. 2020-09-02"
            value={dateText}
            onChange={(e) => setDateText(e.target.value)}
          />
          <button className="go btn" onClick={goToDate}>
            <FaCalendarCheck />
            <span>Go</span>
          </button>
        </span>

        <button
          className="btn"
          onClick={() => dispatch({ type: WeekActionType.NextWeek })}
        >
          <span>Next</span>
          <FaChevronRight />
        </button>
      </p>
    </div>
  );
}
