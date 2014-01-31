package net.emaze.networks.my;

import junit.framework.Assert;
import net.emaze.dysfunctional.order.Order;
import org.junit.Ignore;
import org.junit.Test;

public class IpTest {

    private static final int LOCALHOST_AS_INT = 2130706433;
    private static final Ip ADDRESS = IpParsers.parse("127.0.0.1");
    private static final Ip BEFORE_ADDRESS = IpParsers.parse("127.0.0.0");
    private static final Ip AFTER_ADDRESS = IpParsers.parse("127.0.0.2");

    

    @Test
    public void sameMyIpsAreEqual() {
        final Ip anAddress = IpParsers.parseFromBitsV4(LOCALHOST_AS_INT);
        final Ip sameAddress = IpParsers.parseFromBitsV4(LOCALHOST_AS_INT);
        Assert.assertEquals(anAddress, sameAddress);
    }

    @Test
    public void sameMyIpInDifferentFormatsAreEquals() {
        Assert.assertEquals(IpParsers.parseFromBitsV4(LOCALHOST_AS_INT), IpParsers.parse("127.0.0.1"));
    }

    @Test
    public void differentMyIpsAreNotEqual() {
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
        final Ip anAddress = IpParsers.parseFromBitsV4(1);
        final Ip sameAddress = IpParsers.parseFromBitsV4(1);
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
        final IpPolicy V4 = new IpPolicy.V4();
        Assert.assertEquals(Order.LT.order(), V4.getFirstIp().compareTo(V4.getLastIp()));
    }

    @Test(expected = IllegalArgumentException.class)
    public void comparingWithNullThrows() {
        ADDRESS.compareTo(null);
    }

    @Test
    public void offsetCanYieldAGreaterMyIp() {
        final Ip displaced = ADDRESS.next();
        Assert.assertEquals(AFTER_ADDRESS, displaced);
    }

    @Test
    public void nextOfLastIsItself() {
        final IpPolicy V4 = new IpPolicy.V4();
        Assert.assertEquals(V4.getLastIp(), V4.getLastIp().next());
    }

    @Test
    public void offsetCanYieldALesserMyIp() {
        final Ip displaced = ADDRESS.previous();
        Assert.assertEquals(BEFORE_ADDRESS, displaced);
    }

    @Test
    public void previousOfFirstIsItself() {
        final IpPolicy V4 = new IpPolicy.V4();
        Assert.assertEquals(V4.getFirstIp(), V4.getFirstIp().previous());
    }

    @Test
    public void maskingAnAddressYieldsNetworkPart() {
        final Ip address = IpParsers.parse("192.168.1.123");
        final Mask netmask = new Mask(16, new IpPolicy.V4());
        final Ip expected = IpParsers.parse("192.168.0.0");
        Assert.assertEquals(expected, address.mask(netmask));
    }

    @Test(expected = IllegalArgumentException.class)
    public void maskingWithNullThrows() {
        ADDRESS.mask(null);
    }

    @Test
    @Ignore
    public void toStringRendersCorrectlyWhenHighestBitOn() {
        final IpPolicy V4 = new IpPolicy.V4();
        Assert.assertEquals("255.255.255.255", V4.getLastIp().toString());
    }

    @Test
    @Ignore
    public void toStringRendersCorrectlyWhenHighestBitOff() {
        final IpPolicy V4 = new IpPolicy.V4();
        Assert.assertEquals("0.0.0.0", V4.getFirstIp().toString());
    }
}
