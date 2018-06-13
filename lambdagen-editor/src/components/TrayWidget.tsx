import * as React from "react";

export interface ITrayWidgetProps {
	handleGenerate: () => void;
}

class TrayWidget extends React.Component<ITrayWidgetProps, any> {

	constructor(props: ITrayWidgetProps) {
		super(props);
	}

	public render() {
		return (
			<div className="tray">
				<button className="generate" onClick={this.props.handleGenerate}>
					Generate
				</button>
				{this.props.children}
			</div>
		);
	}
}

export default TrayWidget;