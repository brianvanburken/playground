export interface NatuurlijkPersoonModel {
  id: number;

  bsn?: string;
  geboortedatum?: string;
  geboortegemeente?: string;
  geborenInBuitenland?: boolean;
  geslachtsnaam: string;
  land?: string;
  voornamen: string;
  voorvoegselGeslachtsnaam?: string;
  woonplaats?: string;
}
