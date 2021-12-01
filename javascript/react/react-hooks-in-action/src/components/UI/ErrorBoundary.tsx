import {Component, ReactNode} from 'react';


export interface ErrorBoundaryProps {
  children?: ReactNode;
  fallback?: ReactNode;
}

interface ErrorBoundaryState {
  hasError: boolean;
}

export default class ErrorBoundary extends Component<ErrorBoundaryProps,
  ErrorBoundaryState> {

  constructor(props: ErrorBoundaryProps) {
    super(props);
    this.state = {hasError: false};
  }

  static getDerivedStateFromError() {
    return {hasError: true};
  }

  public render(): ReactNode {
    const {
      children,
      fallback = <h1>Something went wrong.</h1>
    } = this.props;

    return this.state.hasError ? fallback : children;
  }
}
