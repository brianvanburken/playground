import { Component, Input } from "@angular/core";
import { FormControl, FormGroup, Validators } from "@angular/forms";

@Component({
  selector: "app-user-form",
  template: `
    <ng-container [formGroup]="formGroup">
      <input formControlName="name" placeholder="name" />
      <input formControlName="lastName" placeholder="lastName" />
    </ng-container>
  `,
})
export class UserFormComponent {
  @Input() public formGroup!: FormGroup;

  public ngOnInit(): void {
    this.formGroup.addControl(
      "name",
      new FormControl("", [Validators.required, Validators.maxLength(10)])
    );
    this.formGroup.addControl(
      "lastName",
      new FormControl("", [Validators.required])
    );
  }
}
