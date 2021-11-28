export default interface Bookable {
  id: number;
  group: string;
  title: string;
  notes: string;
  sessions: number[];
  days: number[];
}
