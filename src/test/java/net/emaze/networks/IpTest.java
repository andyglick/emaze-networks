package net.emaze.networks;

import junit.framework.Assert;
import net.emaze.dysfunctional.order.Order;
import org.junit.Test;

public class IpTest {

    private static final int LOCALHOST_AS_INT = 2130706433;
    private static final Ip ADDRESS = Ip.parse("127.0.0.1");
    private static final Ip BEFORE_ADDRESS = Ip.parse("127.0.0.0");
    private static final Ip AFTER_ADDRESS = Ip.parse("127.0.0.2");

    @Test
    public void sameIpv4sAreEqual() {
        final Ip anAddress = Ip.fromBits(LOCALHOST_AS_INT);
        final Ip sameAddress = Ip.fromBits(LOCALHOST_AS_INT);
        Assert.assertEquals(anAddress, sameAddress);
    }

    @Test
    public void sameIpInDifferentFormatsAreEquals() {
        Assert.assertEquals(Ip.fromBits(LOCALHOST_AS_INT), Ip.parse("127.0.0.1"));
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
        final Ip anAddress = Ip.fromBits(1);
        final Ip sameAddress = Ip.fromBits(1);
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
        Assert.assertEquals(Order.LT.order(), Ip.FIRST_IP.compareTo(Ip.LAST_IP));
    }

    @Test(expected = IllegalArgumentException.class)
    public void comparingWithNullThrows() {
        ADDRESS.compareTo(null);
    }

    @Test
    public void offsetCanYieldAGreaterIp() {
        final Ip displaced = ADDRESS.next();
        Assert.assertEquals(AFTER_ADDRESS, displaced);
    }

    @Test
    public void nextOfLastIsItself() {
        Assert.assertEquals(Ip.LAST_IP, Ip.LAST_IP.next());
    }

    @Test
    public void offsetCanYieldALesserIp() {
        final Ip displaced = ADDRESS.previous();
        Assert.assertEquals(BEFORE_ADDRESS, displaced);
    }

    @Test
    public void previousOfFirstIsItself() {
        Assert.assertEquals(Ip.FIRST_IP, Ip.FIRST_IP.previous());
    }

    @Test
    public void maskingAnAddressYieldsNetworkPart() {
        final Ip address = Ip.parse("192.168.1.123");
        final Mask netmask = Mask.net(16);
        final Ip expected = Ip.parse("192.168.0.0");
        Assert.assertEquals(expected, address.mask(netmask));
    }

    @Test(expected = IllegalArgumentException.class)
    public void maskingWithNullThrows() {
        ADDRESS.mask(null);
    }

    @Test
    public void toStringRendersCorrectlyWhenHighestBitOn() {
        Assert.assertEquals("255.255.255.255", Ip.LAST_IP.toString());
    }

    @Test
    public void toStringRendersCorrectlyWhenHighestBitOff() {
        Assert.assertEquals("0.0.0.0", Ip.FIRST_IP.toString());
    }
    
    @Test
    public void canonicalNameIsExpected() {
        Assert.assertEquals("net.emaze.networks.Ip", Ip.class.getCanonicalName());
    }
}
