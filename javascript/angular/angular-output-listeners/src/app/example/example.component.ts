import { Component, OnInit, Output, EventEmitter } from "@angular/core";

@Component({
  selector: "app-example",
  template: `
    <span *ngIf="!isInteractive">I'm a static message!</span>
    <span *ngIf="isInteractive">I'm a interactive message!</span>
  `,
})
export class ExampleComponent implements OnInit {
  @Output() myEvent = new EventEmitter<void>();
  isInteractive = false;

  ngOnInit(): void {
    console.log(this.myEvent.observers);
    this.isInteractive = this.myEvent.observers.length > 0;
  }
}
