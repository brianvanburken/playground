import { Component, Prop, h } from '@stencil/core';

@Component({
  tag: 'my-component',
  styleUrl: 'my-component.css',
  shadow: true,
})
export class MyComponent {
  @Prop() fullname: string;

  render() {
    return <div>Hello, World! I'm {this.fullname}</div>;
  }
}
