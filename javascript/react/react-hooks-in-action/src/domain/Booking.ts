export type Session =
  | "Breakfast"
  | "Morning"
  | "Lunch"
  | "Afternoon"
  | "Evening";

export default interface Booking {
  session: Session;
  date: string;
  bookableId: number;
  title: string;
}
