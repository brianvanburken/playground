import User from "../../domain/User";

export interface UserDetailsProps {
  user?: User;
}

export default function UserDetails({ user }: UserDetailsProps) {
  return user ? (
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
  ) : null;
}
