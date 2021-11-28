import Bookable from "../../domain/Bookable";

export interface BookablesState {
  bookables: Bookable[];
  group: string;
  bookableIndex: number;
  hasDetails: boolean;
}

export interface SetGroupAction {
  type: BookableActionType.SetGroup;
  payload: string;
}

export interface SetBookableAction {
  type: BookableActionType.SetBookable;
  payload: number;
}

export interface ToggleHasDetails {
  type: BookableActionType.ToggleHasDetails;
}

export interface NextBookable {
  type: BookableActionType.NextBookable;
}

export const enum BookableActionType {
  SetGroup,
  SetBookable,
  ToggleHasDetails,
  NextBookable,
}

export type BookableAction =
  | SetGroupAction
  | SetBookableAction
  | ToggleHasDetails
  | NextBookable;

export default function reducer(state: BookablesState, action: BookableAction) {
  switch (action.type) {
    case BookableActionType.SetGroup:
      return {
        ...state,
        group: action.payload,
        bookableIndex: 0,
      };
    case BookableActionType.SetBookable:
      return {
        ...state,
        bookableIndex: action.payload,
      };
    case BookableActionType.ToggleHasDetails:
      return {
        ...state,
        hasDetails: !state.hasDetails,
      };
    case BookableActionType.NextBookable:
      const count = state.bookables.filter(
        (b) => b.group === state.group
      ).length;
      return {
        ...state,
        bookableIndex: (state.bookableIndex + 1) % count,
      };
    default:
      return state;
  }
}
