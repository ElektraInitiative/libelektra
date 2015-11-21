import QtQuick 2.2

Canvas {

	property color paintcolor

	onPaint: {
		if (available){
			var ctx = getContext("2d")

			ctx.lineWidth = 1
			ctx.strokeStyle = paintcolor
			ctx.fillStyle = paintcolor

			ctx.beginPath()
			ctx.moveTo(0,0)
			ctx.lineTo(0,height)
			ctx.lineTo(width/2, height/2)
			ctx.closePath()

			ctx.fill()
			ctx.stroke()
		}
	}
}

