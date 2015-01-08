package net.emaze.networks;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import net.emaze.dysfunctional.order.Order;
import org.junit.Assert;
import org.junit.Test;

public class Ipv6Test {

    @Test
    public void canConstructIPv6FromIntegerArray() {
        final Ipv6 ip = Ipv6.fromBits(0xFFFF0000, 0, 0, 1);
        final Ipv6 expected = Ipv6.parse("FFFF::1");
        Assert.assertEquals(expected, ip);
    }

    @Test(expected = IllegalArgumentException.class)
    public void constructingIPv6FromIntegerArrayWithSizeDifferentFrom4Throws() {
        final Ipv6 ip = Ipv6.fromBits(0xFFFF0000, 0, 0, 0, 1);
    }

    @Test
    public void nextIp() {
        final Ipv6 ip = Ipv6.parse("aaaa::ffff:7f00:1");
        final Ipv6 nextIp = ip.next();
        final Ipv6 expected = Ipv6.parse("aaaa::ffff:7f00:2");
        Assert.assertEquals(expected, nextIp);
    }

    @Test
    public void previousIp() {
        final Ipv6 ip = Ipv6.parse("aaaa::ffff:7f00:1");
        final Ipv6 previousIp = ip.previous();
        final Ipv6 expected = Ipv6.parse("aaaa::ffff:7f00:0");
        Assert.assertEquals(expected, previousIp);
    }

    @Test
    public void previousIpOfFirstIpIsFirstIpItSelf() {
        Assert.assertEquals(Ipv6.getFirstIp(), Ipv6.getFirstIp().previous());
    }

    @Test
    public void nextIpOfLastIpIsLastIpItSelf() {
        Assert.assertEquals(Ipv6.getLastIp(), Ipv6.getLastIp().next());
    }

    @Test
    public void nextIpIsGreaterThanPrevious() {
        final Ipv6 ip = Ipv6.parse("aaaa::ffff:7f00:1");
        final Ipv6 nextIp = ip.next();
        Assert.assertEquals(Order.GT, Order.of(nextIp.compareTo(ip)));
    }

    @Test
    public void previousIpIsSmallerThanNext() {
        final Ipv6 ip = Ipv6.parse("aaaa::ffff:7f00:1");
        final Ipv6 previousIp = ip.previous();
        Assert.assertEquals(Order.LT, Order.of(previousIp.compareTo(ip)));
    }

    @Test
    public void toByteArrayReturnsIpRepresentation() {
        final Ipv6 ip = Ipv6.parse("FFFF::1");
        final byte[] representation = {-1, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1};
        Assert.assertArrayEquals(representation, ip.toByteArray());
    }

    @Test
    public void canSerializeAndDeserialize() throws IOException, ClassNotFoundException {
        final Ipv6 value = Ipv6.parse("aaaa::ffff:7f00:1");
        final ByteArrayOutputStream baos = new ByteArrayOutputStream();
        final ObjectOutputStream oos = new ObjectOutputStream(baos);
        oos.writeObject(value);
        final ObjectInputStream ois = new ObjectInputStream(new ByteArrayInputStream(baos.toByteArray()));
        final Object got = ois.readObject();
        Assert.assertEquals(value, got);
    }
}
