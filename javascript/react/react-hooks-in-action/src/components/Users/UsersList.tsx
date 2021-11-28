import { useEffect, useReducer } from "react";
import { FaSpinner } from "react-icons/fa";
import User from "../../domain/User";
import { getData } from "../../utils/api";
import reducer, { initialState, UserActionType } from "./reducer";

export default function UsersList() {
  const [state, dispatch] = useReducer(reducer, initialState);
  const { userIndex, users, isLoading, error } = state;

  const user = users[userIndex];

  useEffect(() => {
    dispatch({ type: UserActionType.FetchUsersRequest });
    getData<User[]>("http://localhost:3001/users")
      .then((users) =>
        dispatch({
          type: UserActionType.FetchUsersSuccess,
          payload: users,
        })
      )
      .catch((error: Error) =>
        dispatch({
          type: UserActionType.FetchUsersError,
          payload: error,
        })
      );
  }, []);

  function setUserIndex(index: number) {
    dispatch({
      type: UserActionType.SetUser,
      payload: index,
    });
  }

  if (error) {
    return <p>{error.message}</p>;
  }

  if (isLoading) {
    return (
      <p>
        <FaSpinner /> Loading users...
      </p>
    );
  }

  return (
    <>
      <ul className="users items-list-nav">
        {users.map((u, i) => (
          <li key={u.id} className={i === userIndex ? "selected" : undefined}>
            <button className="btn" onClick={() => setUserIndex(i)}>
              {u.name}
            </button>
          </li>
        ))}
      </ul>

      {user && (
        <div className="user-details">
          <div className="item">
            <div className="item-header">
              <h2>{user.name}</h2>
            </div>

            <div className="item-details">
              <h3>{user.title}</h3>
              <p>{user.notes}</p>
            </div>
          </div>
        </div>
      )}
    </>
  );
}
