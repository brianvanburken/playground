export type Session =
  | "Breakfast"
  | "Morning"
  | "Lunch"
  | "Afternoon"
  | "Evening";

export default interface Booking {
  session: Session;
  date: string;
  title: string;
  notes?: string;

  bookableId: number;
  bookerId?: number;
}
