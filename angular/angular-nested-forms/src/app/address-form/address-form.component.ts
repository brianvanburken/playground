import { Component, Input, OnInit } from "@angular/core";
import { FormControl, FormGroup, Validators } from "@angular/forms";

@Component({
  selector: "app-address-form",
  template: `
    <ng-container [formGroup]="formGroup">
      <input formControlName="zipcode" placeholder="zipcode" />
      <input formControlName="houseNumber" placeholder="houseNumber" />
    </ng-container>
  `,
})
export class AddressFormComponent implements OnInit {
  @Input() public formGroup!: FormGroup;

  public ngOnInit(): void {
    this.formGroup.addControl("zipcode", new FormControl(""));
    this.formGroup.addControl(
      "houseNumber",
      new FormControl("", [Validators.required])
    );
  }
}
