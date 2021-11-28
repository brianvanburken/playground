import User from "../../domain/User";

export const initialState: UsersState = {
  userIndex: 0,
  users: [],
  isLoading: true,
  error: undefined,
};

export interface UsersState {
  userIndex: number;

  users: User[];
  error?: Error;
  isLoading: boolean;
}

export interface SetUserAction {
  type: UserActionType.SetUser;
  payload: number;
}

export interface FetchUsersRequest {
  type: UserActionType.FetchUsersRequest;
}

export interface FetchUsersSuccess {
  type: UserActionType.FetchUsersSuccess;
  payload: User[];
}

export interface FetchUsersError {
  type: UserActionType.FetchUsersError;
  payload: Error;
}

export const enum UserActionType {
  SetUser,

  FetchUsersRequest,
  FetchUsersSuccess,
  FetchUsersError,
}

export type UserAction =
  | SetUserAction
  | FetchUsersRequest
  | FetchUsersSuccess
  | FetchUsersError;

export default function reducer(state: UsersState, action: UserAction) {
  switch (action.type) {
    case UserActionType.SetUser:
      return {
        ...state,
        userIndex: action.payload,
      };

    case UserActionType.FetchUsersRequest:
      return {
        ...state,
        isLoading: true,
        error: undefined,
        users: [],
      };

    case UserActionType.FetchUsersSuccess:
      return {
        ...state,
        isLoading: false,
        users: action.payload,
      };

    case UserActionType.FetchUsersError:
      return {
        ...state,
        isLoading: false,
        error: action.payload,
      };

    default:
      return state;
  }
}
