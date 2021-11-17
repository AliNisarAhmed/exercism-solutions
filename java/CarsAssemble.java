public class CarsAssemble {

    public double productionRatePerHour(int speed) {
      return speed * 221 * getSuccessRate(speed);
    }

    public int workingItemsPerMinute(int speed) {
        return (int)(this.productionRatePerHour(speed) / 60.0);
    }

    private double getSuccessRate(int speed) {
        if (speed > 0 && speed <= 4) {
            return 1.0;
        } else if (speed >= 5 && speed <= 8) {
            return 0.9;
        } else if (speed == 9) {
            return 0.8;
        } else {
            return 0.77;
        }
    }
}
