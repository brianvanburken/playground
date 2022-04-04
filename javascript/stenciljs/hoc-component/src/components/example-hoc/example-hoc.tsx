import {  Component, Prop, h } from '@stencil/core';

@Component({
  tag: 'example-hoc',
  shadow: true,
})
export class ExampleHoc {
  @Prop() first: string;
  @Prop() middle: string;
  @Prop() last: string;

  private getText(): string {
    return [this.first, this.middle, this.last].join(' ');
  }

  render() {
    return <my-component fullname={this.getText()}></my-component>;
  }
}
