import * as React from "react";

export interface ITrayItemWidgetProps {
	model: any;
	color?: string;
	name: string;
}

function TrayItemWidget({model, color, name}: ITrayItemWidgetProps) {
	return (
		<div
			style={{ borderColor: color }}
			draggable={true}
			onDragStart={event => {
				event.dataTransfer.setData("storm-diagram-node", JSON.stringify(model));
			}}
			className="tray-item"
		>
			{name}
		</div>
	);
}

export default TrayItemWidget;