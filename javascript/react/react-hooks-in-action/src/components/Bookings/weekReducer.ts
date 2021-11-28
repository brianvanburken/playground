import { getWeek } from "../../utils/date-wrangler";

export const enum WeekActionType {
  NextWeek,
  PrevWeek,
  Today,
  SetDate,
}

export interface NextWeekAction {
  type: WeekActionType.NextWeek;
}

export interface PrevWeekAction {
  type: WeekActionType.PrevWeek;
}

export interface TodayAction {
  type: WeekActionType.Today;
}

export interface SetDateAction {
  type: WeekActionType.SetDate;
  payload: string;
}

export type WeekAction =
  | NextWeekAction
  | PrevWeekAction
  | TodayAction
  | SetDateAction;

export interface WeekState {
  date: Date;
  start: Date;
  end: Date;
}

export default function reducer(state: WeekState, action: WeekAction) {
  switch (action.type) {
    case WeekActionType.NextWeek:
      return getWeek(state.date, 7);
    case WeekActionType.PrevWeek:
      return getWeek(state.date, -7);
    case WeekActionType.Today:
      return getWeek(new Date());
    case WeekActionType.SetDate:
      return getWeek(new Date(action.payload));
  }
}
