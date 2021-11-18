import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class TestTrack {

    public static void race(RemoteControlCar car) {
        System.out.println("Racing Remote Control Car");
    }

    public static List<ProductionRemoteControlCar> getRankedCars(ProductionRemoteControlCar prc1,
                                                                 ProductionRemoteControlCar prc2) {
        List<ProductionRemoteControlCar> result = new ArrayList<ProductionRemoteControlCar>() {{
            add(prc1);
            add(prc2);
        }};

        result.sort(null);

        return result;
    }
}
