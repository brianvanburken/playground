import { useQuery } from "react-query";
import User from "../../domain/User";
import { getData } from "../../utils/api";
import Avatar from "./Avatar";

export interface UserDetailsProps {
  userId: number;
}

export default function UserDetails({ userId }: UserDetailsProps) {
  const { data: user } = useQuery<User>(
    ["user", userId],
    () => getData<User>(`http://localhost:3001/users/${userId}`),
    { suspense: true }
  );

  return user ? (
    <div className="item user">
      <div className="item-header">
        <h2>{user.name}</h2>
      </div>

      <Avatar
        src={`http://localhost:3001/img/${user.img}`}
        fallbackSrc="http://localhost:3001/img/avatar.gif"
        alt={user.name}
      />

      <div className="item-details">
        <h3>{user.title}</h3>
        <p>{user.notes}</p>
      </div>
    </div>
  ) : null;
}
