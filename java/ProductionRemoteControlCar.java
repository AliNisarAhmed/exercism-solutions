class ProductionRemoteControlCar implements RemoteControlCar, Comparable<ProductionRemoteControlCar> {

    private int numberOfVictories;

    public void drive() {
        System.out.println("Driving Production Remote Car");
    }

    public int getDistanceTravelled() {
        return 10;
    }

    public int getNumberOfVictories() {
        return this.numberOfVictories;
    }

    public void setNumberOfVictories(int numberofVictories) {
        this.numberOfVictories = numberofVictories;
    }

    @Override
    public int compareTo(ProductionRemoteControlCar o) {
        return this.getNumberOfVictories() - o.getNumberOfVictories();
    }
}
