public class ExperimentalRemoteControlCar implements RemoteControlCar {

    public void drive() {
        System.out.println("Driving Experimental Car");
    }

    public int getDistanceTravelled() {
        return 30;
    }
}
