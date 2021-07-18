import { Component } from "@angular/core";

@Component({
  selector: "app-root",
  template: `
    <app-example (myEvent)="log($event)"></app-example>
    <app-example></app-example>
  `,
  styles: [],
})
export class AppComponent {
  log($event: any) {
    console.log($event);
  }
}
