import * as React from "react";

class TrayWidget extends React.Component<any, any> {

	constructor(props: any) {
		super(props);
	}

	public render() {
		return <div className="tray">{this.props.children}</div>;
	}
}

export default TrayWidget;