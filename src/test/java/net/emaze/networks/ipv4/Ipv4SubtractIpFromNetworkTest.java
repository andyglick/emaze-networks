package net.emaze.networks.ipv4;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;
import org.junit.Assert;
import org.junit.Test;

public class Ipv4SubtractIpFromNetworkTest {

    public static final Ipv4SubtractIpFromNetwork subtractor = new Ipv4SubtractIpFromNetwork();

    @Test
    public void excludingIpOutsideCidrYieldsCidr() {
        final Ipv4Network cidr = Ipv4Network.fromCidrNotation("255.0.0.0", 30);
        final Set<Ipv4Network> got = subtractor.perform(cidr, Ipv4.parse("10.0.0.0"));
        Assert.assertEquals(Collections.singleton(cidr), got);
    }

    @Test
    public void excludingAllCidrContentsYieldsEmptySet() {
        final Ipv4Network cidr = Ipv4Network.fromCidrNotation("255.0.0.0", 32);
        final Set<Ipv4Network> got = subtractor.perform(cidr, Ipv4.parse("255.0.0.0"));
        Assert.assertEquals(Collections.emptySet(), got);
    }

    @Test
    public void excludingFirstIpYieldsExpected() {
        final Ipv4Network cidr = Ipv4Network.fromCidrNotation("255.0.0.0", 30);
        final Set<Ipv4Network> got = subtractor.perform(cidr, Ipv4.parse("255.0.0.0"));
        final Set<Ipv4Network> expected = new HashSet<>(Arrays.asList(
                Ipv4Network.fromCidrNotation("255.0.0.1", 32),
                Ipv4Network.fromCidrNotation("255.0.0.2", 31)));
        org.junit.Assert.assertEquals(expected, got);
    }

    @Test
    public void excludingMiddleIpYieldsExpected() {
        final Ipv4Network cidr = Ipv4Network.fromCidrNotation("255.0.0.0", 30);
        final Set<Ipv4Network> got = subtractor.perform(cidr, Ipv4.parse("255.0.0.2"));
        final Set<Ipv4Network> expected = new HashSet<>(Arrays.asList(
                Ipv4Network.fromCidrNotation("255.0.0.0", 31),
                Ipv4Network.fromCidrNotation("255.0.0.3", 32)));
        org.junit.Assert.assertEquals(expected, got);
    }

    @Test
    public void excludingLastIpYieldsExpected() {
        final Ipv4Network cidr = Ipv4Network.fromCidrNotation("255.0.0.0", 30);
        final Set<Ipv4Network> got = subtractor.perform(cidr, Ipv4.parse("255.0.0.3"));
        final Set<Ipv4Network> expected = new HashSet<>(Arrays.asList(
                Ipv4Network.fromCidrNotation("255.0.0.0", 31),
                Ipv4Network.fromCidrNotation("255.0.0.2", 32)));
        org.junit.Assert.assertEquals(expected, got);
    }
}
