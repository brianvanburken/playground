import DeleteIcon from "@mui/icons-material/Delete";
import EditIcon from "@mui/icons-material/Edit";
import { IconButton, ListItem, ListItemText } from "@mui/material";
import { NatuurlijkPersoonModel } from "./NatuurlijkPersoonModel";

interface NatuurlijkPersoonListItemProps {
  persoon: NatuurlijkPersoonModel;
  deletePersoon: (_: NatuurlijkPersoonModel) => void;
  editPersoon: (_: NatuurlijkPersoonModel) => void;
}

export default function NatuurlijkPersoonListItem({
  persoon,
  deletePersoon,
  editPersoon,
}: NatuurlijkPersoonListItemProps) {
  return (
    <>
      <ListItem
        secondaryAction={
          <>
            <IconButton onClick={() => deletePersoon(persoon)}>
              <DeleteIcon />
            </IconButton>
            <IconButton onClick={() => editPersoon(persoon)}>
              <EditIcon />
            </IconButton>
          </>
        }
      >
        <ListItemText
          primary={getPersoonnaam(persoon)}
          secondary={getWoonplaats(persoon)}
        />
      </ListItem>
    </>
  );
}

function getPersoonnaam(persoon: NatuurlijkPersoonModel): string {
  return `${persoon.voornamen}${
    persoon.voorvoegselGeslachtsnaam
      ? " " + persoon.voorvoegselGeslachtsnaam
      : ""
  } ${persoon.geslachtsnaam}`;
}

function getWoonplaats(persoon: NatuurlijkPersoonModel): string {
  return `${persoon.woonplaats}, ${persoon.land}`;
}
