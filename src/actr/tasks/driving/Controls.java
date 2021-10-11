// package actr.tasks.driving;

// // import net.java.games.input.Controller;
// // import net.java.games.input.ControllerEnvironment;
// // import net.java.games.input.Component;
// // import net.java.games.input.Component.Identifier;

// public class Controls {

//     Controller steering_wheel = null;

//     public void startUp() {
//         Controller[] controllers = ControllerEnvironment.getDefaultEnvironment().getControllers();

//         // Find the steering wheel
//         for (int i = 0; i < controllers.length && steering_wheel == null; i++) {
//             if (controllers[i].getType() == Controller.Type.STICK) {
//                 steering_wheel = controllers[i];
//                 break;
//             }
//         }

//         if (steering_wheel == null) {
//             System.out.println("Found no steering wheel!");
//             System.exit(0);
//         }
//     }

//     public double getAngle() {
//         double xAxis = 0;
//         steering_wheel.poll();
//         Component[] components = steering_wheel.getComponents();
//         Component steeringAngle = null;

//         // find the component that logs the steering angle (X-Axis)
//         for (int i = 0; i < components.length && steeringAngle == null; i++) {
//             Component component = components[i];
//             Identifier componentIdentifier = component.getIdentifier();
//             if (componentIdentifier == Component.Identifier.Axis.X) {
//                 steeringAngle = component;
//                 break;
//             }
//         }

//         if (steeringAngle.isAnalog())
//             xAxis = steeringAngle.getPollData();
//         try {
//             Thread.sleep(20);
//         } catch (InterruptedException e) {
//             e.printStackTrace();
//         }
//         return xAxis;
//     }
// }
