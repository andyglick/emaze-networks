package net.emaze.networks;

import net.emaze.dysfunctional.order.Order;
import org.junit.Assert;
import org.junit.Test;

public class Ipv4Test {

    private static final int LOCALHOST_AS_INT = 2130706433;
    private static final Ipv4 ADDRESS = Ipv4.parse("127.0.0.1");
    private static final Ipv4 BEFORE_ADDRESS = Ipv4.parse("127.0.0.0");
    private static final Ipv4 AFTER_ADDRESS = Ipv4.parse("127.0.0.2");

    @Test
    public void sameIpv4sAreEqual() {
        final Ipv4 anAddress = Ipv4.fromBits(LOCALHOST_AS_INT);
        final Ipv4 sameAddress = Ipv4.fromBits(LOCALHOST_AS_INT);
        Assert.assertEquals(anAddress, sameAddress);
    }

    @Test
    public void sameIpInDifferentFormatsAreEquals() {
        Assert.assertEquals(Ipv4.fromBits(LOCALHOST_AS_INT), Ipv4.parse("127.0.0.1"));
    }

    @Test
    public void differentIpv4sAreNotEqual() {
        Assert.assertEquals(false, ADDRESS.equals(AFTER_ADDRESS));
    }

    @Test
    public void ipv4IsDifferentFromNull() {
        Assert.assertEquals(false, ADDRESS.equals(null));
    }

    @Test
    public void ipv4IsDifferentFromOtherObjects() {
        Assert.assertEquals(false, ADDRESS.equals(new Object()));
    }

    @Test
    public void comparingWithSameYieldsEqual() {
        final Ipv4 anAddress = Ipv4.fromBits(1);
        final Ipv4 sameAddress = Ipv4.fromBits(1);
        Assert.assertEquals(Order.EQ.order(), anAddress.compareTo(sameAddress));
    }

    @Test
    public void comparingWithNextYieldsGreaterThan() {
        Assert.assertEquals(Order.GT.order(), AFTER_ADDRESS.compareTo(ADDRESS));
    }

    @Test
    public void comparingWithPreviousYieldsLesserThan() {
        Assert.assertEquals(Order.LT.order(), BEFORE_ADDRESS.compareTo(ADDRESS));
    }

    @Test
    public void firstIsLessThanLast() {
        Assert.assertEquals(Order.LT.order(), Ipv4.getFirstIp().compareTo(Ipv4.getLastIp()));
    }

    @Test(expected = IllegalArgumentException.class)
    public void comparingWithNullThrows() {
        ADDRESS.compareTo(null);
    }

    @Test
    public void offsetCanYieldAGreaterIp() {
        final Ipv4 displaced = ADDRESS.next();
        Assert.assertEquals(AFTER_ADDRESS, displaced);
    }

    @Test
    public void nextOfLastIsItself() {
        Assert.assertEquals(Ipv4.getLastIp(), Ipv4.getLastIp().next());
    }

    @Test
    public void offsetCanYieldALesserIp() {
        final Ipv4 displaced = ADDRESS.previous();
        Assert.assertEquals(BEFORE_ADDRESS, displaced);
    }

    @Test
    public void previousOfFirstIsItself() {
        Assert.assertEquals(Ipv4.getFirstIp(), Ipv4.getFirstIp().previous());
    }

    @Test
    public void maskingAnAddressYieldsNetworkPart() {
        final Ipv4 address = Ipv4.parse("192.168.1.123");
        final Ipv4Mask netmask = Ipv4Mask.net(16);
        final Ipv4 expected = Ipv4.parse("192.168.0.0");
        Assert.assertEquals(expected, address.mask(netmask));
    }

    @Test(expected = IllegalArgumentException.class)
    public void maskingWithNullThrows() {
        ADDRESS.mask(null);
    }

    @Test
    public void toStringRendersCorrectlyWhenHighestBitOn() {
        Assert.assertEquals("255.255.255.255", Ipv4.getLastIp().toString());
    }

    @Test
    public void toStringRendersCorrectlyWhenHighestBitOff() {
        Assert.assertEquals("0.0.0.0", Ipv4.getFirstIp().toString());
    }
}
