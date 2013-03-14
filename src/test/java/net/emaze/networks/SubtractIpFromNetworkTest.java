package net.emaze.networks;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;
import junit.framework.Assert;
import org.junit.Test;

public class SubtractIpFromNetworkTest {

    public static final SubtractIpFromNetwork subtractor = new SubtractIpFromNetwork();

    @Test
    public void excludingIpOutsideCidrYieldsCidr() {
        final Network cidr = Network.fromCidrNotation("255.0.0.0", 30);
        final Set<Network> got = subtractor.perform(cidr, Ip.parse("10.0.0.0"));
        Assert.assertEquals(Collections.singleton(cidr), got);
    }

    @Test
    public void excludingAllCidrContentsYieldsEmptySet() {
        final Network cidr = Network.fromCidrNotation("255.0.0.0", 32);
        final Set<Network> got = subtractor.perform(cidr, Ip.parse("255.0.0.0"));
        Assert.assertEquals(Collections.emptySet(), got);
    }

    @Test
    public void excludingFirstIpYieldsExpected() {
        final Network cidr = Network.fromCidrNotation("255.0.0.0", 30);
        final Set<Network> got = subtractor.perform(cidr, Ip.parse("255.0.0.0"));
        final Set<Network> expected = new HashSet<>(Arrays.asList(
                Network.fromCidrNotation("255.0.0.1", 32),
                Network.fromCidrNotation("255.0.0.2", 31)));
        org.junit.Assert.assertEquals(expected, got);
    }

    @Test
    public void excludingMiddleIpYieldsExpected() {
        final Network cidr = Network.fromCidrNotation("255.0.0.0", 30);
        final Set<Network> got = subtractor.perform(cidr, Ip.parse("255.0.0.2"));
        final Set<Network> expected = new HashSet<>(Arrays.asList(
                Network.fromCidrNotation("255.0.0.0", 31),
                Network.fromCidrNotation("255.0.0.3", 32)));
        org.junit.Assert.assertEquals(expected, got);
    }

    @Test
    public void excludingLastIpYieldsExpected() {
        final Network cidr = Network.fromCidrNotation("255.0.0.0", 30);
        final Set<Network> got = subtractor.perform(cidr, Ip.parse("255.0.0.3"));
        final Set<Network> expected = new HashSet<>(Arrays.asList(
                Network.fromCidrNotation("255.0.0.0", 31),
                Network.fromCidrNotation("255.0.0.2", 32)));
        org.junit.Assert.assertEquals(expected, got);
    }
}
