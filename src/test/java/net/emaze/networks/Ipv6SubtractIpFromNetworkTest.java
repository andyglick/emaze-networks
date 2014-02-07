package net.emaze.networks;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;
import org.junit.Assert;
import org.junit.Test;

public class Ipv6SubtractIpFromNetworkTest {

    public static final Ipv6SubtractIpFromNetwork subtractor = new Ipv6SubtractIpFromNetwork();

    @Test
    public void excludingIpOutsideCidrYieldsCidr() {
        final Ipv6Network cidr = Ipv6Network.fromCidrNotation("1234::/16");
        final Set<Ipv6Network> got = subtractor.perform(cidr, Ipv6.parse("4321::"));
        Assert.assertEquals(Collections.singleton(cidr), got);
    }

    @Test
    public void excludingAllCidrContentsYieldsEmptySet() {
        final Ipv6Network cidr = Ipv6Network.fromCidrNotation("1234::1/128");
        final Set<Ipv6Network> got = subtractor.perform(cidr, Ipv6.parse("1234::1"));
        Assert.assertEquals(Collections.emptySet(), got);
    }

    @Test
    public void excludingFirstIpYieldsExpected() {
        final Ipv6Network cidr = Ipv6Network.fromCidrNotation("1234::", 126);
        final Set<Ipv6Network> got = subtractor.perform(cidr, Ipv6.parse("1234::"));
        final Set<Ipv6Network> expected = new HashSet<>(Arrays.asList(
                Ipv6Network.fromCidrNotation("1234::1", 128),
                Ipv6Network.fromCidrNotation("1234::2", 127)));
        org.junit.Assert.assertEquals(expected, got);
    }

    @Test
    public void excludingMiddleIpYieldsExpected() {
        final Ipv6Network cidr = Ipv6Network.fromCidrNotation("1234::", 126);
        final Set<Ipv6Network> got = subtractor.perform(cidr, Ipv6.parse("1234::1"));
        final Set<Ipv6Network> expected = new HashSet<>(Arrays.asList(
                Ipv6Network.fromCidrNotation("1234::", 128),
                Ipv6Network.fromCidrNotation("1234::2", 127)));
        org.junit.Assert.assertEquals(expected, got);
    }

    @Test
    public void excludingLastIpYieldsExpected() {
        final Ipv6Network cidr = Ipv6Network.fromCidrNotation("1234::", 126);
        final Set<Ipv6Network> got = subtractor.perform(cidr, Ipv6.parse("1234::3"));
        final Set<Ipv6Network> expected = new HashSet<>(Arrays.asList(
                Ipv6Network.fromCidrNotation("1234::2", 128),
                Ipv6Network.fromCidrNotation("1234::", 127)));
        org.junit.Assert.assertEquals(expected, got);
    }
}
