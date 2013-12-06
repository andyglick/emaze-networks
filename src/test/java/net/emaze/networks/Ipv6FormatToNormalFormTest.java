package net.emaze.networks;

import org.junit.Assert;
import org.junit.Ignore;
import org.junit.Test;

public class Ipv6FormatToNormalFormTest {

    private final Ipv6FormatToNormalForm instance = new Ipv6FormatToNormalForm();

    @Test
    public void leavesImmutatedAlreadyNormalizedAddress() {
        final String address = "0123:4567:89ab:cdef:fedc:ba98:7654:3210";
        Assert.assertEquals(address, instance.perform(address));
    }

    @Test
    public void normalizesUndefinedAddress() {
        final String address = "::";
        final String expected = "0000:0000:0000:0000:0000:0000:0000:0000";
        Assert.assertEquals(expected, instance.perform(address));
    }

    @Test
    public void normalizesAddressStartingWithDoubleColon() {
        final String address = "::ffff";
        final String expected = "0000:0000:0000:0000:0000:0000:0000:ffff";
        Assert.assertEquals(expected, instance.perform(address));
    }

    @Test
    public void normalizesAddressEndingWithDoubleColon() {
        final String address = "ffff::";
        final String expected = "ffff:0000:0000:0000:0000:0000:0000:0000";
        Assert.assertEquals(expected, instance.perform(address));
    }

    @Test
    public void addsPaddingZeroes() {
        final String address = "0:1:2:3:4:5:6:7";
        final String expected = "0000:0001:0002:0003:0004:0005:0006:0007";
        Assert.assertEquals(expected, instance.perform(address));
    }

    @Test(expected = IllegalArgumentException.class)
    public void cannotContainMoreThanOneDoubleColons() {
        final String address = "ffff::aaaa::ffff";
        instance.perform(address);
    }

    @Test(expected = IllegalArgumentException.class)
    public void cannotContainSequencesOfMoreThanTwoColons() {
        final String address = "ffff:::aaaa";
        instance.perform(address);
    }
}
