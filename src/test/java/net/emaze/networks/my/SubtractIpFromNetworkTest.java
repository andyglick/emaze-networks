package net.emaze.networks.my;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;
import org.junit.Assert;
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
        Assert.assertEquals(expected, got);
    }

    @Test
    public void excludingMiddleIpYieldsExpected() {
        final Network cidr = Network.fromCidrNotation("255.0.0.0", 30);
        final Set<Network> got = subtractor.perform(cidr, Ip.parse("255.0.0.2"));
        final Set<Network> expected = new HashSet<>(Arrays.asList(
                Network.fromCidrNotation("255.0.0.0", 31),
                Network.fromCidrNotation("255.0.0.3", 32)));
        Assert.assertEquals(expected, got);
    }

    @Test
    public void excludingLastIpYieldsExpected() {
        final Network cidr = Network.fromCidrNotation("255.0.0.0", 30);
        final Set<Network> got = subtractor.perform(cidr, Ip.parse("255.0.0.3"));
        final Set<Network> expected = new HashSet<>(Arrays.asList(
                Network.fromCidrNotation("255.0.0.0", 31),
                Network.fromCidrNotation("255.0.0.2", 32)));
        Assert.assertEquals(expected, got);
    }

    
    @Test
    public void excludingIpOutsideCidrYieldsCidrV6() {
        final Network cidr = Network.fromCidrNotation("2001:0DB8:0000:CD31::", 64);
        final Set<Network> got = subtractor.perform(cidr, Ip.parse("2002:0DB8:0000:CD31::"));
        Assert.assertEquals(Collections.singleton(cidr), got);
    }

    @Test
    public void excludingAllCidrContentsYieldsEmptySetV6() {
        final Network cidr = Network.fromCidrNotation("2001:0DB8:0000:CD31::", 128);
        final Set<Network> got = subtractor.perform(cidr, Ip.parse("2001:0DB8:0000:CD31::"));
        Assert.assertEquals(Collections.emptySet(), got);
    }

    @Test
    public void excludingFirstIpYieldsExpectedV6() {
        final Network cidr = Network.fromCidrNotation("2001:0DB8:0000:CD31::FFFC", 126);
        final Set<Network> got = subtractor.perform(cidr, Ip.parse("2001:0DB8:0000:CD31::FFFC"));
        final Set<Network> expected = new HashSet<>(Arrays.asList(
                Network.fromCidrNotation("2001:0DB8:0000:CD31::FFFE", 127),
                Network.fromCidrNotation("2001:0DB8:0000:CD31::FFFD", 128)));
        Assert.assertEquals(expected, got);
    }

    @Test
    public void excludingMiddleIpYieldsExpectedV6() {
        final Network cidr = Network.fromCidrNotation("2001:0DB8:0000:CD31::FFFC", 126);
        final Set<Network> got = subtractor.perform(cidr, Ip.parse("2001:0DB8:0000:CD31::FFFD"));
        final Set<Network> expected = new HashSet<>(Arrays.asList(
                Network.fromCidrNotation("2001:0DB8:0000:CD31::FFFE", 127),
                Network.fromCidrNotation("2001:0DB8:0000:CD31::FFFC", 128)));
        Assert.assertEquals(expected, got);
    }

    @Test
    public void excludingLastIpYieldsExpectedV6() {
        final Network cidr = Network.fromCidrNotation("2001:0DB8:0000:CD31::FFFC", 126);
        final Set<Network> got = subtractor.perform(cidr, Ip.parse("2001:0DB8:0000:CD31::FFFF"));
        final Set<Network> expected = new HashSet<>(Arrays.asList(
                Network.fromCidrNotation("2001:0DB8:0000:CD31::FFFC", 127),
                Network.fromCidrNotation("2001:0DB8:0000:CD31::FFFE", 128)));
        Assert.assertEquals(expected, got);
    }
}