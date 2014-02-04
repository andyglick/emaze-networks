package net.emaze.networks.jackson;

import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.IOException;
import junit.framework.Assert;
import net.emaze.networks.jackson.IpToStringTest.BeanWithIp;
import net.emaze.networks.Ip;
import org.junit.Test;

public class IpFromStringTest {

    @Test
    public void deserializingYieldsExpected() throws IOException {
        final String serialized = "{'ip':'127.0.0.1'}".replace("'", "\"");
        final Ip expected = Ip.parse("127.0.0.1");
        final ObjectMapper mapper = new ObjectMapper();
        mapper.registerModule(new NetworksModule());
        final BeanWithIp got = mapper.readValue(serialized, BeanWithIp.class);
        Assert.assertEquals(expected, got.getIp());
    }

    @Test
    public void deserializingIPv6YieldsExpected() throws IOException {
        final String serialized = "{'ip':'1234:0000:0000:0000:0000:0000:0000:0001'}".replace("'", "\"");
        final Ip expected = Ip.parse("1234::1");
        final ObjectMapper mapper = new ObjectMapper();
        mapper.registerModule(new NetworksModule());
        final BeanWithIp got = mapper.readValue(serialized, BeanWithIp.class);
        Assert.assertEquals(expected, got.getIp());
    }
}
