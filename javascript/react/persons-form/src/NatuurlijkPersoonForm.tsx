import { yupResolver } from "@hookform/resolvers/yup";
import { CloseOutlined } from "@mui/icons-material";
import {
  Autocomplete,
  Button,
  Card,
  CardContent,
  CardHeader,
  Checkbox,
  FormControlLabel,
  Grid,
  IconButton,
  TextField,
} from "@mui/material";
import { Controller, useForm } from "react-hook-form";
import * as yup from "yup";
import countries from "./landen.json";
import { NatuurlijkPersoonModel } from "./NatuurlijkPersoonModel";

declare module "yup" {
  interface DateSchema {
    transformDutchFormat(): DateSchema;
  }
}

yup.addMethod(yup.date, "transformDutchFormat", function () {
  return this.transform(function (_value: unknown, originalValue: string) {
    return /\d{2}-\d{2}-\d{4}/.test(originalValue)
      ? new Date(originalValue.split("-").reverse().join("-"))
      : originalValue;
  });
});

const geboortegemeentes = [
  "Rhenen",
  "Veenendaal",
  "Amerongen",
  "Arnhem",
  "Utrecht",
];
const voorvoegsels = ["van", "der", "van der"];
const landen = countries.map((c) => c.label);

interface NatuurlijkPersoonFormProps {
  editingPersoon?: NatuurlijkPersoonModel;
  addPersoon: (_: NatuurlijkPersoonModel) => void;
  hideForm: () => void;
}

export default function NatuurlijkPersoonForm({
  editingPersoon,
  addPersoon,
  hideForm,
}: NatuurlijkPersoonFormProps) {
  const date16YearsAgo = new Date();
  date16YearsAgo.setFullYear(date16YearsAgo.getFullYear() - 16);

  const schema = yup.object().shape(
    {
      bsn: yup
        .string()
        .notRequired()
        .when("bsn", {
          is: isFilledIn,
          then: yup
            .string()
            .min(8, "BSN moet minimaal 8 cijfers bevatten.")
            .max(9, "BSN moet minimaal 9 cijfers bevatten.")
            .trim("BSN mag alleen cijfers bevatten.")
            .matches(/^[0-9]+$/, {
              excludeEmptyString: true,
              message: "BSN mag alleen cijfers bevatten.",
            })
            .test("elfproef", "BSN moet voldoen aan de elfproef.", elfproef),
          otherwise: yup.string().nullable(),
        }),

      voornamen: yup
        .string()
        .required("Voornamen is vereist.")
        .max(200, "Voornamen moet ingevuld zijn."),

      voorvoegselGeslachtsnaam: yup
        .string()
        .notRequired()
        .when("voorvoegselGeslachtsnaam", {
          is: isFilledIn,
          then: yup
            .string()
            .max(10, "Voorvoegsels mag maximaal 10 karakters bevatten.")
            .trim("Voorvoegsels dient ingevuld te zijn.")
            .oneOf(
              voorvoegsels,
              "Voorvoegsel komt niet voor in lijst met bekende voorvoegsels."
            ),
          otherwise: yup.string().nullable(),
        }),
      geslachtsnaam: yup
        .string()
        .max(200, "Geslachtsnaam mag maximaal 200 karakters bevatten.")
        .required("Geslachtsnaam is vereist."),

      woonplaats: yup
        .string()
        .max(50, "Woonplaats mag maximaal 50 karakters bevatten.")
        .required("Woonplaats is vereist."),

      land: yup.string().required("Land is vereist."),

      geboortedatum: yup
        .date()
        .transformDutchFormat()
        .typeError("Geboortedatum moet een valide datum zijn.")
        .required("Geboortedatum is vereist.")
        .max(date16YearsAgo, "Een persoon moet minimaal 16 jaar zijn."),

      geboortegemeente: yup
        .string()
        .when("geborenInBuitenland", {
          is: false,
          then: yup
            .string()
            .required("Geboortegemeente is vereist.")
            .trim("Geboortegemeente dient ingevuld te zijn.")
            .oneOf(
              geboortegemeentes,
              "Geboortegemeente komt niet voor in lijst met bekende gemeentes."
            ),
          otherwise: yup.string().notRequired().nullable(),
        })
        .max(240, "Geboortegemeente mag maximaal 240 karakters bevatten."),
      geborenInBuitenland: yup.bool(),
    },
    [
      ["bsn", "bsn"],
      ["voorvoegselGeslachtsnaam", "voorvoegselGeslachtsnaam"],
    ]
  );

  const defaultValues = {
    id: new Date().getTime() + Math.random(),
    bsn: "",
    geboortedatum: "",
    geboortegemeente: "",
    geborenInBuitenland: false,
    geslachtsnaam: "",
    land: "Netherlands",
    voornamen: "",
    voorvoegselGeslachtsnaam: "",
    woonplaats: "",
    ...(editingPersoon || {}),
  };

  console.log({ editingPersoon, defaultValues });

  const {
    control,
    register,
    handleSubmit,
    formState: { errors },
  } = useForm<NatuurlijkPersoonModel>({
    resolver: yupResolver(schema),
    mode: "onChange",
    defaultValues,
  });

  const transformBeforeSubmit = (p: NatuurlijkPersoonModel) => {
    console.log({ p });
    p.geboortedatum = ((p.geboortedatum as Date | undefined) || new Date())
      .toISOString()
      .split("T")[0]
      .split("-")
      .reverse()
      .join("-");
    addPersoon(p);
  };

  return (
    <Card>
      <CardContent>
        <CardHeader
          action={
            <IconButton onClick={() => hideForm()}>
              <CloseOutlined />
            </IconButton>
          }
          title={!!editingPersoon ? "Persoon aanpassen" : "Persoon toevoegen"}
        />

        <form onSubmit={handleSubmit(transformBeforeSubmit)} noValidate>
          <Grid container spacing={2}>
            <Grid item xs={12} md={12}>
              <TextField
                error={!!errors.bsn}
                label="BSN"
                helperText={errors.bsn?.message || "Optioneel"}
                fullWidth
                {...register("bsn")}
              />
            </Grid>

            <Grid item xs={12} md={5}>
              <TextField
                error={!!errors.voornamen}
                label="Voornamen"
                helperText={errors.voornamen?.message}
                fullWidth
                {...register("voornamen")}
              />
            </Grid>

            <Grid item xs={12} md={2}>
              <Autocomplete
                options={voorvoegsels}
                renderInput={(params) => (
                  <TextField
                    {...params}
                    label="Voorvoegsels"
                    error={!!errors.voorvoegselGeslachtsnaam}
                    helperText={
                      errors.voorvoegselGeslachtsnaam?.message || "Optioneel"
                    }
                    {...register("voorvoegselGeslachtsnaam")}
                  />
                )}
              />
            </Grid>

            <Grid item xs={12} md={5}>
              <TextField
                error={!!errors.geslachtsnaam}
                label="Geslachtsnaam"
                helperText={errors.geslachtsnaam?.message}
                fullWidth
                {...register("geslachtsnaam")}
              />
            </Grid>

            <Grid item xs={12} md={12}>
              <TextField
                error={!!errors.woonplaats}
                label="Woonplaats"
                helperText={errors.woonplaats?.message}
                fullWidth
                {...register("woonplaats")}
              />
            </Grid>

            <Grid item xs={12} md={12}>
              <Controller
                control={control}
                name="land"
                render={({ field: { onChange, value } }) => (
                  <Autocomplete
                    onChange={(_event, item) => onChange(item)}
                    value={value}
                    options={landen}
                    freeSolo
                    autoHighlight
                    renderInput={(params) => (
                      <TextField
                        {...params}
                        error={!!errors.land}
                        fullWidth
                        helperText={errors.land?.message}
                        label="Land"
                      />
                    )}
                  />
                )}
              />
            </Grid>

            <Grid item xs={12} md={12}>
              <Controller
                control={control}
                name="geboortegemeente"
                render={({ field: { onChange, value } }) => (
                  <Autocomplete
                    onChange={(_event, item) => onChange(item)}
                    value={value}
                    options={geboortegemeentes}
                    freeSolo
                    renderInput={(params) => (
                      <TextField
                        {...params}
                        error={!!errors.geboortegemeente}
                        fullWidth
                        helperText={errors.geboortegemeente?.message}
                        label="Geboortegemeente"
                      />
                    )}
                  />
                )}
              />
            </Grid>

            <Grid item xs={12} md={12}>
              <TextField
                error={!!errors.geboortedatum}
                label="Geboortedatum"
                placeholder="dd-mm-jjjj"
                helperText={errors.geboortedatum?.message}
                fullWidth
                {...register("geboortedatum")}
              />
            </Grid>

            <Grid item xs={12} md={12}>
              <FormControlLabel
                control={
                  <Controller
                    name="geborenInBuitenland"
                    control={control}
                    render={({ field: { onChange, value } }) => (
                      <Checkbox
                        checked={value}
                        onChange={(e) => onChange(e.target.checked)}
                      />
                    )}
                  />
                }
                label="Geboren in buitenland?"
              />
            </Grid>

            <Grid item xs="auto">
              <Button onClick={() => hideForm()} variant="outlined">
                Annuleren
              </Button>
            </Grid>

            <Grid item xs="auto">
              <Button type="submit" variant="contained">
                Opslaan
              </Button>
            </Grid>
          </Grid>
        </form>
      </CardContent>
    </Card>
  );
}

function elfproef(bsn?: string): boolean {
  bsn = bsn?.trim()?.padStart(9, "0") ?? "";
  let sum = 0;
  for (let i = 0, l = bsn.length; i < l; i++) {
    sum += parseInt(bsn[i], 10) * (i === l - 1 ? -1 : l - i);
  }
  return sum % 11 === 0;
}

function isFilledIn(v: string | undefined) {
  return (v?.length ?? 0) > 0;
}
