import { Component } from "@angular/core";
import { FormControl, FormGroup } from "@angular/forms";

@Component({
  selector: "app-root",
  template: `
    <form [formGroup]="form" (ngSubmit)="submit($event)">
      <app-user-form [formGroup]="user"></app-user-form>
      <app-address-form [formGroup]="address"></app-address-form>

      <button>Submit</button>
    </form>
  `,
})
export class AppComponent {
  public form = new FormGroup({
    user: new FormGroup({}),
    address: new FormGroup({}),
  });

  public submit(event: Event): void {
    event.preventDefault();
    console.log("submit form invalid", this.form.invalid);
    this.form.markAllAsTouched();
    return;
  }

  get address(): FormGroup {
    return this.form.controls.address as FormGroup;
  }

  get user(): FormGroup {
    return this.form.controls.user as FormGroup;
  }
}
