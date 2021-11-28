import WeekPicker from "./WeekPicker";

export default function BookingsPage() {
  return (
    <main className="bookings-page">
      <WeekPicker date={new Date()} />
    </main>
  );
}
