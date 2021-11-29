import Bookable from "../../domain/Bookable";

export const initialState: BookablesState = {
  group: "Rooms",
  bookableIndex: 0,
  bookables: [],
  isLoading: true,
  error: undefined,
};

export interface BookablesState {
  group: string;
  bookableIndex: number;

  bookables: Bookable[];
  error?: Error;
  isLoading: boolean;
}

export interface SetGroupAction {
  type: BookableActionType.SetGroup;
  payload: string;
}

export interface SetBookableAction {
  type: BookableActionType.SetBookable;
  payload: number;
}

export interface NextBookable {
  type: BookableActionType.NextBookable;
}

export interface FetchBookablesRequest {
  type: BookableActionType.FetchBookablesRequest;
}

export interface FetchBookablesSuccess {
  type: BookableActionType.FetchBookablesSuccess;
  payload: Bookable[];
}

export interface FetchBookablesError {
  type: BookableActionType.FetchBookablesError;
  payload: Error;
}

export const enum BookableActionType {
  SetGroup,
  SetBookable,
  NextBookable,

  FetchBookablesRequest,
  FetchBookablesSuccess,
  FetchBookablesError,
}

export type BookableAction =
  | SetGroupAction
  | SetBookableAction
  | NextBookable
  | FetchBookablesRequest
  | FetchBookablesSuccess
  | FetchBookablesError;

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
    case BookableActionType.NextBookable:
      const count = state.bookables.filter(
        (b) => b.group === state.group
      ).length;
      return {
        ...state,
        bookableIndex: (state.bookableIndex + 1) % count,
      };

    case BookableActionType.FetchBookablesRequest:
      return {
        ...state,
        isLoading: true,
        error: undefined,
        bookables: [],
      };

    case BookableActionType.FetchBookablesSuccess:
      return {
        ...state,
        isLoading: false,
        bookables: action.payload,
      };

    case BookableActionType.FetchBookablesError:
      return {
        ...state,
        isLoading: false,
        error: action.payload,
      };
    default:
      return state;
  }
}
