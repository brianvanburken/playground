import { FaSpinner } from "react-icons/fa";

interface SpinnerProps {
  [key: string]: unknown;
}

export default function Spinner(props: SpinnerProps) {
  return (
    <span {...props}>
      <FaSpinner className="icon-loading" />
    </span>
  );
}
