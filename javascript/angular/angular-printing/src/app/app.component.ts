import { Component, HostListener } from "@angular/core";

@Component({
  selector: "app-root",
  template: `
    <button (click)="print($event)">Print</button>
    <span *ngIf="isPrinting">I'm only visible when printing</span>
  `,
})
export class AppComponent {
  public isPrinting = false;

  @HostListener("window:beforeprint")
  public onBeforePrint(): void {
    this.isPrinting = true;
  }

  @HostListener("window:afterprint")
  public onAfterPrint(): void {
    // AfterPrint is normally triggered instantly. By wrapping it in an
    // setTimeout it gets put onto the call-stack in JavaScript and executed
    // after the print dialog closes and JavaScript is resumed.
    setTimeout(() => {
      this.isPrinting = false;
    }, 1);
  }

  public print(event: MouseEvent): void {
    event.preventDefault();
    this.isPrinting = true;
    // By wrapping the print method in a setTimeout we add it to the
    // call-stack and forces the runtime to pause after opening
    // the print dialog.
    setTimeout(() => {
      window.print();
    }, 0);
  }
}
